(ns server.core
  (:use [ring.middleware stacktrace params nested-params keyword-params file file-info session]
        [clojure.tools.logging])
  (:require [compojure.core :refer [defroutes GET]]
            [org.httpkit.server :refer [run-server with-channel websocket? on-close on-receive send!]]
            [server.messages :refer [handle-message]]
            [clojure.tools.reader.edn :as edn]
            [ring.util.response :refer [response]]))

#_(def channel-hub (atom {}))

#_(defn save-channel [sess-id channel]
    (swap! channel-hub assoc sess-id channel)
    (info "channel-hub count " (count @channel-hub))
    (info "channel-hub count " @channel-hub)
  )

(defn get-session-id [req]
  (-> req :cookies (get "ring-session") :value))

(defn open-channel [req]
  (let [sess-id (get-session-id req)]
    (info sess-id)
    (with-channel req channel 
      #_(save-channel sess-id channel)
      (if (websocket? channel)
        (info "open-channel WebSocket channel")
        (info "open-channel HTTP channel"))
      (on-close channel (fn [status]
                          (info "channel closed")
                          #_(swap! channel-hub dissoc sess-id)))
      (on-receive channel (fn [data] 
                            (let [res (if (= data "ping") 
                                        data
                                        (pr-str (handle-message (edn/read-string data) channel)))]
                              (send! channel res))))
      channel)))

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
      wrap-session
      ))

(defn -main [port & opts]
  (run-server #'app {:port (Integer. port) :join? false}))

;(def srvr (-main 8080))
;(srvr)
