(ns server.mail
  (:import [java.util Properties]
           [javax.mail Session Store Folder]))
            
(def empanda 
  {:protocol "imaps"
   :server "mail.empanda.net"
   })

(defn get-store [user passwd]
  (let [props (doto (Properties.)
                (.put "mail.imap.ssl.checkserveridentity" "false")
                (.put "mail.imaps.ssl.trust" "mail.empanda.net")
                #_(.put "mail.debug" "true"))]
    (let [session (Session/getDefaultInstance props)
          store (doto (.getStore session "imaps")
                  (.connect "mail.empanda.net" user passwd))]
      store)))

(defn inetaddr->map [inetaddr]
  (select-keys (bean inetaddr) [:personal :address]))

(defn as-message [msg]
  (let [b (bean msg)]
    {:subject (:subject b) :sender (inetaddr->map (:sender b))
     :content (.substring (:content b) 0 100)
     :sent (:sentDate b)}))

(defn newest-messages [store]
  (let [inbox (doto
                (.getFolder store "INBOX")
                (.open Folder/READ_ONLY))
        max-id (.getMessageCount inbox)
        msgs (reverse (into [] (.getMessages inbox (max 0 (- max-id 0)) max-id)))]
    (map as-message msgs)))

;(def em-store (get-store "harry" "passwd"))
;(newest-messages em-store)
