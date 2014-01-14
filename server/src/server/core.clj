(ns server.core
  (:use [ring.middleware stacktrace params nested-params keyword-params file file-info]
        [clojure.tools.logging])
  (:require [compojure.core :refer [defroutes GET]]
            [org.httpkit.server :refer [run-server with-channel websocket? on-close on-receive send!]]
            [server.messages :refer [handle-message]]
            [clojure.tools.reader.edn :as edn]
            [ring.util.response :refer [response]]))

(defn get-session-id [req]
  (-> req :cookies (get "ring-session") :value))

(defn open-channel [req]
  (with-channel req channel 
    (if (websocket? channel)
      (info "open-channel WebSocket channel")
      (info "open-channel HTTP channel"))
    (on-close channel (fn [status]
                        (info "channel closed")
                        (handle-message {:type :close} channel)))
    (on-receive channel (fn [data] 
                          (let [res (if (= data "ping") 
                                      data
                                      (pr-str (handle-message (edn/read-string data) channel)))]
                            (send! channel res))))
    channel))

(defroutes app*
  (GET "/ws" req (open-channel req))
  (GET "/req" req (pr-str req))
  )

(def app
  (-> app*
      wrap-stacktrace
      wrap-keyword-params
      wrap-params
      wrap-nested-params
      (wrap-file "public" {:allow-symlinks? true})
      wrap-file-info
      ))

(defn -main [port & opts]
  (run-server #'app {:port (Integer. port) :join? false}))

;(def srvr (-main 8080))
;(srvr)
