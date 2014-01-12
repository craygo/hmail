(ns server.core
  (:use [ring.middleware stacktrace params nested-params keyword-params file file-info]
        [clojure.tools.logging])
  (:require [compojure.core :refer [defroutes GET]]
            [org.httpkit.server :refer [run-server with-channel websocket? on-close on-receive send!]]))

(def channel-hub (atom {}))

(defn save-channel [user channel]
    (swap! channel-hub assoc (str (:_id user)) channel)
    (info "channel-hub count " (count @channel-hub)))

(defn open-channel [req]
  (let [user {:id 1}]  ; TODO
    (with-channel req channel 
      (save-channel user channel)
      (if (websocket? channel)
        (info "open-channel WebSocket channel")
        (info "open-channel HTTP channel"))
      (on-close channel (fn [status]
                          (info "channel closed")
                          (swap! channel-hub dissoc (str (:_id user)))))
      (on-receive channel (fn [data] 
                            (info "received " data)
                            (send! channel data))))))

(defroutes app*
  (GET "/ws" req (open-channel req))
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
