(ns server.javamail
  (:use [clojure.tools.logging])
  (:import [java.util Properties]
           [javax.mail Session Store Folder Flags Flags$Flag FetchProfile FetchProfile$Item]))
            
(defn get-store 
  ([user passwd]
   (get-store user passwd "mail.empanda.net"))
  ([user passwd server]
   (let [props (doto (Properties.)
                 (.put "mail.imap.ssl.checkserveridentity" "false")
                 (.put "mail.imaps.ssl.trust" server)
                 #_(.put "mail.debug" "true"))]
     (let [session (Session/getInstance props)
           store (doto (.getStore session "imaps")
                   (.connect server user passwd))]
       store))))

(defn inetaddr->map [inetaddr]
  (select-keys (bean inetaddr) [:personal :address]))

(defn content-handler [b]
  (let [cont (:content b)]
    (condp #(.startsWith %2 %1) (:contentType b)
      "TEXT/PLAIN" {:type :plain :content cont}
      "TEXT/HTML" {:type :html :content cont}
      "multipart/ALTERNATIVE" (content-handler (bean (.getBodyPart cont 1)))
      "multipart/MIXED" (content-handler (bean (.getBodyPart cont 0)))
      "multipart/RELATED" (content-handler (bean (.getBodyPart cont 0)))
      cont)))

(defn msg->map [msg]
  (let [b (bean msg)]
    {:subject (:subject b) :sender (inetaddr->map (:sender b))
     :content nil #_(content-handler b)
     :sent (:sentDate b)
     :id (:messageNumber b)
     :flags (let [flags (-> b :flags bean :systemFlags)]
              (areduce flags i ret #{} 
                       (conj ret
                             (condp = (aget flags i)
                               Flags$Flag/ANSWERED :answered
                               Flags$Flag/DELETED :deleted
                               Flags$Flag/DRAFT :draft
                               Flags$Flag/FLAGGED :flagged
                               Flags$Flag/RECENT :recent
                               Flags$Flag/SEEN :seen
                               Flags$Flag/USER :user
                               ))))}))

(defn get-inbox [store]
  (doto (.getFolder store "INBOX")
    (.open Folder/READ_WRITE)))

(defn prefetch-messages [folder n offs]
  (let [max-id (.getMessageCount folder)
        msgs (.getMessages folder (max 0 (- max-id (dec n) offs)) (- max-id offs))
        fp (doto (FetchProfile.)
             (.add FetchProfile$Item/ENVELOPE)
             (.add FetchProfile$Item/FLAGS)
             (.add FetchProfile$Item/CONTENT_INFO))]
    (.fetch folder msgs fp)
    (->> msgs (into []) reverse)))

(defn fetch-content [folder mesg-num]
  (let [mesg (.getMessage folder mesg-num)
        b (bean mesg)]
    (content-handler b)))

(defn message-count [folder]
  (.getMessageCount folder))

(defn newer-than [folder msg-num]
  (let [newer-cnt (- (message-count folder) msg-num)]
    (if (pos? newer-cnt)
      (prefetch-messages folder newer-cnt 0))))

(defn- to-jm-flag [flag]
  (case flag
    :deleted (Flags. Flags$Flag/DELETED)
    :seen (Flags. Flags$Flag/SEEN)
  ))

(defn set-flags [folder msg-nums flag bool]
  (.setFlags folder (int-array msg-nums) (to-jm-flag flag) bool))

(defn- parse-type [i]
  (case i
    1 :messages
    2 :folders))

(defn get-store-folders [store]
  (into {} (map #(vector (.getName %) {:holds (parse-type (.getType %)) :jm-folder %}) (.list (.getDefaultFolder store)))))
