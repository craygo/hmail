(ns server.messages
  (:use [clojure.tools.logging])
  (:require [server.mail :refer [get-store newest-messages]])
  )

(defonce store (delay (get-store "harry" "passwd")))

(def messages (atom {}))

(defn init-messages []
  (if (empty? @messages)
    (reset! messages (newest-messages @store)))
  ;(info (pr-str @messages))
  @messages)

(defn handle-message [{:keys [type topic] :as mesg}]
  (condp = type
    :init (init-messages)
    (pr-str type)
    ))
