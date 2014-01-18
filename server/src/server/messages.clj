(ns server.messages
  (:use [clojure.tools.logging])
  (:require [server.mail :refer [get-store prefetch-messages fetch-content as-message]]
            [clojure.core.async :refer [go chan <! >! <!!]]
            [org.httpkit.server :refer [send!]]
            [server.images :refer [stop-images]]
            ))

(def channels (atom {})) ; channel to {:store :messages}

(defmacro ws-send [channel & body]
  `(go (send! ~channel (pr-str (do ~@body)))))

;; message helpers
(defn messages-mesg [channel & [init]]
  {:type (if init :init :merge) :topic [:messages] :value (get-in @channels [channel :client-messages-by-id])})

(defn fetch-content-for-all-messages [channel]
  ;(info "fetch-content-for-all-messages " channel)
  (let [msgs (get-in @channels [channel :raw-messages])]
    (doseq [mesg msgs]
      (go (let [[id content] (fetch-content mesg)
                content (stop-images content)]
            ;(info "fetch-content-for-all-messages " id (count content))
            (swap! channels assoc-in [channel :client-messages-by-id id :content] content)
            (send! channel (pr-str (messages-mesg channel))))))))

(def prefetch-size 10)

(defn prefetch-top-messages [channel]
  (let [store (get-in @channels [channel :store])
        msgs (prefetch-messages store prefetch-size)
        client-msgs (map as-message msgs)
        client-msgs (into {} (map #(vector (:id %) %) client-msgs))]
    (swap! channels assoc-in [channel :raw-messages] msgs)
    (swap! channels assoc-in [channel :client-messages-by-id] client-msgs)
    (fetch-content-for-all-messages channel)))

(defn have-messages? [channel]
  (seq (get-in @channels [channel :client-messages-by-id])))

;; handlers
(defn init-messages [channel]
  (if-not (have-messages? channel)
    (do
      (ws-send channel 
               (prefetch-top-messages channel)
               (messages-mesg channel :init))
      [])
    (messages-mesg channel)))

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

(defn logout [channel]
  (swap! channels assoc-in [channel] nil)
  {:type :update :topic [:user] :value {}})

(defn close [channel]
  (swap! channels dissoc channel))

(defn handle-message [{:keys [type topic] :as mesg} channel]
  (try
    ;(info "handle-message " mesg)
    (condp = type
      :init (init-messages channel)
      :login (login mesg channel)
      :logout (logout channel)
      :close (close channel)
      (info "no handler for: " (:dissoc mesg :value))
      )
    (catch Exception e
      (error e e))))
