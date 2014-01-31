(ns server.mail
  (:use [clojure.tools.logging])
  (:require [server.javamail :refer [get-store get-inbox prefetch-messages msg->map fetch-content newer-than set-flags get-store-folders]]))

; cache of mail keyed by client-id
; structure is {:store :folders}
; folder is map {:name {:jm-folder :messages}}
; message is map {:id {:id :subject :sender :sent :content :flags}}

(def cache (atom {}))

(defn login 
  ""
  [cid username password server]
  (let [store (get-store username password server)]
    (swap! cache assoc-in [cid :store] store)
    (swap! cache update-in [cid :folders "INBOX"] merge {:jm-folder (get-inbox store) :holds :messages})))

(defn- close-store [cid]
  (try
    (if-let [store (get-in @cache [cid :store])]
      (if (.isConnected store)
        (.close store)))
    (catch Exception e
      (warn "close-store cid " cid e))))

(defn logout [cid]
  (close-store cid)
  (swap! cache assoc-in [cid] {}))

(defn close [cid]
  (close-store cid)
  (swap! cache dissoc cid))

(defn- map-by [sq k]
  (into {} (map #(vector (get % k) %) sq)))

(defn- merge-messages [cid folder-name msgs]
  (let [msgs (map msg->map msgs)
        msgs (map-by msgs :id)]
    (swap! cache update-in [cid :folders folder-name :messages] merge msgs)
    msgs))

(defn have-messages? [cid folder-name]
  (not (empty? (get-in @cache [cid :folders folder-name :messages]))))

(defn- get-jm-folder [cid folder-name]
  (get-in @cache [cid :folders folder-name :jm-folder]))

(defn prefetch 
  "Prefetch messages from the folder, cache and return them"
  [cid folder-name n offs & [from]]
  (if-let [folder (get-jm-folder cid folder-name)]
    (let [msgs (prefetch-messages folder n offs from)]
      (merge-messages cid folder-name msgs))
    (warn "prefetch no folder for " folder-name)))

(defn get-content 
  "Returns and caches content for the message with msg-num."
  [cid folder-name msg-num]
  (if-let [folder (get-jm-folder cid folder-name)]
    (let [content (fetch-content folder msg-num)]
      (swap! cache assoc-in [cid :folders folder-name :messages msg-num :content] content)
      content)))

(defn check-new 
  "Checks for new messages in the folder and prefetches them.
  Returns new messages."
  [cid folder-name]
  (if-let [folder (get-jm-folder cid folder-name)]
    (let [curr-max-num (apply max (keys (get-in @cache [cid :folders folder-name :messages])))
          msgs (newer-than folder curr-max-num)]
      (merge-messages cid folder-name msgs))))

(defn- do-delete [folder msg-nums]
  (info "do-delete " folder msg-nums)
  )

(defn flag-with
  "Flag message with nums with given flag and return map of changed messages"
  [cid folder-name msg-nums flag]
  (info "flag-with " flag)
  (when-let [folder (get-jm-folder cid folder-name)]
    (let [[flag bool] (first flag)]
      (set-flags folder msg-nums flag bool)
      (if (= :deleted flag)
        (do-delete folder msg-nums))
      (swap! cache update-in [cid :folders folder-name :messages]
             #(reduce (fn [m msg-num] (update-in m [msg-num :flags] (if bool conj disj) flag))
                      % msg-nums))
      (into {} (filter #(contains? (set msg-nums) (key %1))
                       (get-in @cache [cid :folders folder-name :messages]))))))

(defn get-folders 
  "Gets folders, caches and returns the names"
  [cid]
  (if-let [store (get-in @cache [cid :store])]
    (if (<= (count (get-in @cache [cid :folders])) 1)
      (let [folders (get-store-folders store)
            folders (dissoc folders "INBOX")]
        (swap! cache update-in [cid :folders]
               #(reduce (fn [m [folder-name folder-map]] 
                          (if-not (contains? m folder-name)
                            (assoc m folder-name folder-map)
                            m))
                        % folders))
        (into {} (map (fn [[k v]] (vector k (assoc (dissoc v :jm-folder) :messages {}))) folders))))))
