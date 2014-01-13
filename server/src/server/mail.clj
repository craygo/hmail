(ns server.mail
  (:use [clojure.tools.logging])
  (:import [java.util Properties]
           [javax.mail Session Store Folder]))
            
(def empanda 
  {:protocol "imaps"
   :server "mail.empanda.net"
   })

(defn get-store 
  ([user passwd]
   (get-store user passwd "mail.empanda.net"))
  ([user passwd server]
   (let [props (doto (Properties.)
                 (.put "mail.imap.ssl.checkserveridentity" "false")
                 (.put "mail.imaps.ssl.trust" server)
                 #_(.put "mail.debug" "true"))]
     (let [session (Session/getDefaultInstance props)
           store (doto (.getStore session "imaps")
                   (.connect server user passwd))]
       store))))

(defn inetaddr->map [inetaddr]
  (select-keys (bean inetaddr) [:personal :address]))

(defn content-handler [b]
  (let [cont (:content b)]
    (info "content-handler " (:contentType b) (type (:contentType b)) (type cont))
    (condp #(.startsWith %2 %1) (:contentType b)
      "TEXT/HTML" cont
      "multipart/ALTERNATIVE" (.getContent (.getBodyPart cont 1)) ; TODO check part 1 is HTML
      "multipart/MIXED" "multipart/MIXED" ;(.getContent (.getBodyPart cont 1)) ; TODO check part 1 is HTML
      cont)))

(defn as-message [msg]
  (let [b (bean msg)]
    {:subject (:subject b) :sender (inetaddr->map (:sender b))
     :content (content-handler b)
     :sent (:sentDate b)
     :id (:messageNumber b)}))

(defn newest-messages [store]
  (let [inbox (doto
                (.getFolder store "INBOX")
                (.open Folder/READ_ONLY))
        max-id (.getMessageCount inbox)
        msgs (reverse (into [] (.getMessages inbox (max 0 (- max-id 3)) (- max-id 1))))]
    (map as-message msgs)))

;(def em-store (get-store "harry" "passwd"))
;(newest-messages em-store)
