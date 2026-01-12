(ns enion-backend.db
  (:require
    [clojure.string :as str]
    [clojure.walk :as walk]
    [java-time :as jt]
    [mount.core :as mount]
    [nano-id.core :refer [nano-id]])
  (:import
    (java.nio.charset
      StandardCharsets)
    (java.security
      MessageDigest)
    (java.util
      Collection
      Map)
    (java.util.concurrent
      Executors)))

;; In-Memory DB State
(mount/defstate ^{:on-reload :noop} db
                :start (atom {:users {} :purchases {}}))

(defonce ^:private single-thread-executor (Executors/newSingleThreadExecutor))

(defn run-async
  [{:keys [f on-success on-failed]}]
  (.submit
    single-thread-executor
    (fn []
      (try
        (let [result (f)]
          (when on-success (on-success result)))
        (catch Throwable t
          (when on-failed (on-failed t)))))))

(defn- gen-uuid []
  (str (random-uuid)))

(defn- create-auth-token []
  (apply str (repeatedly 5 nano-id)))

(defn- current-date []
  (jt/format "yyyy-MM-dd'T'HH:mm" (jt/zoned-date-time (jt/zone-id "UTC"))))

(defn vals-key->str [m]
  (->> m
       (map (fn [[k v]]
              [(name k) (str (namespace v) "/" (name v))]))
       (into {})))

(defn- get-users []
  (:users @db))

(defn- get-purchases []
  (:purchases @db))

(defn create-user [cg-user-id username prev-user-data end-time]
  (let [auth-token (create-auth-token)
        user-id (gen-uuid)
        created-at (current-date)
        user-data (merge {"auth" auth-token
                          "coins" 0
                          "equipped" {}
                          "cg_user_id" cg-user-id
                          "username" username
                          "created_at" created-at}
                         {"equipped" (vals-key->str (:equipped prev-user-data {}))
                          "created_at" (:created_at prev-user-data created-at)
                          "coins" (:coins prev-user-data 0)})
        ;; Simulate applying boosters immediately
        user-data (reduce 
                    (fn [d b] (assoc d (name b) end-time))
                    user-data 
                    [:booster_regen_mana :booster_defense :booster_damage])]
    (swap! db assoc-in [:users user-id] user-data)
    {:auth auth-token
     :uid user-id}))

(defn get-user-purchases [user-id]
  (->> (get-purchases)
       vals
       (filter #(= (get % "owner_id") user-id))
       (map #(keyword (get % "item")))
       set))

(defn- get-user-data* [user-id user-data]
  (when user-data
    (-> user-data
        (assoc :purchases (get-user-purchases user-id)
               :player-id user-id)
        (walk/keywordize-keys)
        (update :equipped (fn [e]
                            (into {} (map (fn [[k v]]
                                            [(keyword k) (keyword v)]) e)))))))

(defn get-user-data [auth-token]
  (let [user-entry (first (filter #(= (get (val %) "auth") auth-token) (get-users)))]
    (when user-entry
      (get-user-data* (key user-entry) (val user-entry)))))

(defn get-user-data-by-cg-user-id [cg-user-id]
  (let [user-entry (first (filter #(= (get (val %) "cg_user_id") cg-user-id) (get-users)))]
    (when user-entry
      (get-user-data* (key user-entry) (val user-entry)))))

(defn get-user-coin [user-id]
  (get-in @db [:users user-id "coins"] 0))

(defn update-username [user-id username]
  (swap! db assoc-in [:users user-id "username"] username))

(defn get-username [user-id]
  (get-in @db [:users user-id "username"]))

(defn purchase [user-id price item-id rewarded?]
  (when-not rewarded?
    (swap! db update-in [:users user-id "coins"] - price))
  (let [purchase-id (gen-uuid)]
    (swap! db assoc-in [:purchases purchase-id]
           {"owner_id" user-id
            "item" item-id
            "level" 1
            "date" (current-date)})))

(defn apply-booster [user-id booster-db-name end-time]
  (swap! db assoc-in [:users user-id booster-db-name] end-time))

(defn add-coins [user-id boost? on-success]
  (run-async
    {:f (fn []
          (swap! db update-in [:users user-id "coins"] + (if boost? 20 10)))
     :on-success on-success}))

(defn equip-item [user-id new-item-id type]
  (swap! db assoc-in [:users user-id "equipped" type] new-item-id))

(defn sha-256 [original-string]
  (let [digest (MessageDigest/getInstance "SHA-256")
        hashed-bytes (.digest digest (.getBytes original-string StandardCharsets/UTF_8))]
    (format "%064x" (BigInteger. 1 hashed-bytes))))

(defn check-username-exists [username]
  (let [username-lower-case (str/lower-case username)
        exists? (some #(= (get (val %) "username_lower_case") username-lower-case) (get-users))]
    (when exists?
      (throw (ex-info "Username exists!" {})))))

(defn sign-up [update? player-data username password]
  (let [user-id (:player-id player-data)
        current-username (:username player-data)
        username-lower-case (str/lower-case username)
        password-hash (sha-256 password)
        new-auth-token (create-auth-token)]
    (when-not (and update?
                   current-username
                   username
                   (= (str/lower-case current-username) username-lower-case))
      (check-username-exists username))
    
    (swap! db update-in [:users user-id] merge 
           {"username" username
            "username_lower_case" username-lower-case
            "password" password-hash
            "account" true
            "auth" new-auth-token})
    (get-user-data new-auth-token)))

(defn log-in [username password]
  (let [username-lower-case (str/lower-case username)
        password-hash (sha-256 password)
        user-entry (first (filter (fn [[_ u]]
                                    (and (= (get u "username_lower_case") username-lower-case)
                                         (= (get u "password") password-hash)))
                                  (get-users)))]
    (when-not user-entry
      (throw (ex-info "Log-in failed. Please check your username and password, then try again." {})))
    
    (let [user-id (key user-entry)
          new-auth-token (create-auth-token)]
      (swap! db update-in [:users user-id] assoc 
             "auth" new-auth-token
             "updated_at" (current-date))
      (get-user-data new-auth-token))))

(defn update-log-in-time [user-id]
  (swap! db update-in [:users user-id] 
         (fn [u]
           (-> u
               (assoc "updated_at" (current-date))
               (update "number-of-plays" (fnil inc 0))))))

(defn update-cg-username [user-id username]
  (swap! db update-in [:users user-id] assoc
         "username" username
         "username_lower_case" (str/lower-case username)))
