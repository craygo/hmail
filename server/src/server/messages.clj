(ns server.messages
  (:use [clojure.tools.logging])
  (:require [server.mail :refer [get-store newest-messages]]
            [clojure.core.async :refer [go chan <! >! <!!]]
            [org.httpkit.server :refer [send!]]
            ))

(def channels (atom {}))

(defmacro ws-send [channel & body]
  `(go (send! ~channel (<! (go (pr-str (do ~@body)))))))

(defn init-messages [channel]
  (info "init-messages " channel #_(get-in @channels [channel :messages]))
  (if (empty? (get-in @channels [channel :messages]))
    (do
      (ws-send channel 
               (swap! channels assoc-in [channel :messages] (newest-messages (get-in @channels [channel :store])))
               {:type :init :topic [:messages] :value (get-in @channels [channel :messages])})
      [])
    {:type :init :topic [:messages] :value (get-in @channels [channel :messages])}))

(defn login [mesg channel]
  (try 
    (let [value (:value mesg)
          {:keys [username password server]} value
          store* (get-store username password server)]
      (info "login " username server)
      (if (.isConnected store*)
        (do
          (info "login connected")
          (swap! channels assoc-in [channel  :store] store*)
          {:type :update :topic [:user] :value {:name username}})
        (do
          (info "login NOT connected")
          {:type :update :topic [:user] :value {:msg "failed"}})))
    (catch javax.mail.AuthenticationFailedException e
      (info "login " e)
      {:type :update :topic [:user] :value {:msg (.getMessage e)}})
    (catch Exception e
      (error e e)
      {:type :update :topic [:user] :value {:msg (.getMessage e)}})))

(defn logout [mesg channel]
  (info "logout " channel)
  (swap! channels assoc-in [channel] nil)
  {:type :update :topic [:user] :value {}})

(defn handle-message [{:keys [type topic] :as mesg} channel]
  (try
    ;(info "handle-message " mesg)
    (condp = type
      :init (init-messages channel)
      :login (login mesg channel)
      :logout (logout mesg channel)
      (info "no handler for: " (:dissoc mesg :value))
      )
    (catch Exception e
      (error e e))))
