(ns server.messages
  (:use [clojure.tools.logging])
  (:require [server.mail :refer [get-store get-inbox prefetch-messages fetch-content as-message message-count]]
            [clojure.core.async :refer [go chan <! >! <!!]]
            [org.httpkit.server :refer [send!]]
            [server.images :refer [stop-images]]
            ))

(def channels (atom {})) ; channel to {:store :messages}

#_(-> @channels
    first 
    second
    :store
    (.getFolder "INBOX")
    .isOpen
    )

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

(defn prefetch-top-messages [channel n]
  (let [inbox (get-in @channels [channel :inbox])]
    (info "prefetch-top-messages inbox " inbox)
    (let [msgs (prefetch-messages inbox n)
          client-msgs (map as-message msgs)
          client-msgs (into {} (map #(vector (:id %) %) client-msgs))]
      (swap! channels update-in [channel :raw-messages] concat msgs)
      (swap! channels update-in [channel :client-messages-by-id] merge client-msgs)
      (fetch-content-for-all-messages channel))))

(defn have-messages? [channel]
  (seq (get-in @channels [channel :client-messages-by-id])))

(defn check-new [channel inbox]
  (let [msg-cnt (message-count inbox)
        highest (apply max (keys (get-in @channels [channel :client-messages-by-id])))]
    (if (> msg-cnt highest)
      (prefetch-top-messages channel (- msg-cnt highest)))
    (messages-mesg channel)))

(def prefetch-size 3)

;; handlers
(defn init-messages [channel]
  (if-not (have-messages? channel)
    (do
      (ws-send channel 
               (prefetch-top-messages channel prefetch-size)
               (messages-mesg channel :init))
      [])
    (check-new channel (get-in @channels [channel :inbox]))))

(defn login [mesg channel]
  (try 
    (let [value (:value mesg)
          {:keys [username password server]} value
          store* (get-store username password server)]
      (info "login " username server)
      (if (.isConnected store*)
        (let [inbox (get-inbox store*)]
          (info "login connected inbox " inbox)
          (swap! channels assoc-in [channel :store] store*)
          (swap! channels assoc-in [channel :inbox] inbox)
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
