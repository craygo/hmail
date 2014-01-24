(ns server.mail
  (:use [clojure.tools.logging])
  (:require [server.javamail :refer [get-store get-inbox prefetch-messages msg->map fetch-content newer-than]]))

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
    (swap! cache assoc-in [cid :folders "INBOX" :jm-folder ] (get-inbox store))))

(defn logout [cid]
  (swap! cache assoc-in [cid] {}))

(defn close [cid]
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

(defn prefetch 
  "Prefetch messages from the folder, cache and return them"
  [cid folder-name n offs]
  (if-let [folder (get-in @cache [cid :folders folder-name :jm-folder])]
    (let [msgs (prefetch-messages folder n offs)]
      (merge-messages cid folder-name msgs))
    (warn "prefetch no folder for " folder-name)))

(defn get-content 
  "Returns and caches content for the message with msg-num."
  [cid folder-name msg-num]
  (if-let [folder (get-in @cache [cid :folders folder-name :jm-folder])]
    (let [content (fetch-content folder msg-num)]
      (swap! cache assoc-in [cid :folders folder-name :messages msg-num :content] content)
      content)))

(defn check-new 
  "Checks for new messages in the folder and prefetches them.
  Returns new messages."
  [cid folder-name]
  (if-let [folder (get-in @cache [cid :folders folder-name :jm-folder])]
    (let [curr-max-num (apply max (keys (get-in @cache [cid :folders folder-name :messages])))
          msgs (newer-than folder curr-max-num)]
      (merge-messages cid folder-name msgs))))

(defn- reset [cid folder-name]
  (swap! cache assoc-in [cid :folders folder-name :messages] {}))
