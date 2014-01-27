(ns server.messages
  (:use [clojure.tools.logging])
  (:require [server.mail :as mail :refer [have-messages? get-content prefetch check-new flag-with]]
            [clojure.core.async :refer [go chan <! >! <!!]]
            [org.httpkit.server :refer [send!]]
            [server.images :refer [stop-images]]
            ))

(defmacro ws-send [channel & body]
  `(go (send! ~channel (pr-str (do ~@body)))))

;; message helpers
(defn messages-mesg [msgs & [init]]
  {:type (if init :init :merge) :topic [:messages] :value msgs})

(defn fetch-content-for-messages [channel msgs]
  (let [msg-nums (keys msgs)]
    (doseq [msg-num msg-nums]
      (go (let [content (get-content channel "INBOX" msg-num)
                content (stop-images content)]
            (send! channel (pr-str (messages-mesg {msg-num {:content content}}))))))))

(defn prefetch-top-messages [channel n]
  (let [msgs (prefetch channel "INBOX" n 0)]
    (fetch-content-for-messages channel msgs)
    msgs))

(def prefetch-size 10)

;; handlers
(defn init-messages [channel]
  (if-not (have-messages? channel "INBOX")
    (do
      (ws-send channel 
               (let [msgs (prefetch-top-messages channel prefetch-size)]
                 (messages-mesg msgs :init)))
      [])
    (let [msgs (check-new channel "INBOX")]
      (fetch-content-for-messages channel msgs)
      (messages-mesg msgs))))

(defn login [mesg channel]
  (try 
    (let [value (:value mesg)
          {:keys [username password server]} value]
      (info "login " username server)
      (mail/login channel username password server)
      {:type :update :topic [:user] :value {:name username}})
    (catch javax.mail.AuthenticationFailedException e
      (info "login " e)
      {:type :update :topic [:user] :value {:msg (.getMessage e)}})
    (catch Exception e
      (error e e)
      {:type :update :topic [:user] :value {:msg (.getMessage e)}})))

(defn logout [channel]
  (mail/logout channel)
  {:type :update :topic [:user] :value {}})

(defn close [channel]
  (mail/close channel))

(defn mark [mesg channel]
  (let [{:keys [msg-nums flag]} (:value mesg)
        msgs (flag-with channel "INBOX" msg-nums flag)
        msgs (into {} (map (fn [[k v]] [k {:flags (:flags v)}]) msgs))]
    (messages-mesg msgs)))

(defn handle-message [{:keys [type topic] :as mesg} channel]
  (try
    ;(info "handle-message " mesg)
    (condp = type
      :init (init-messages channel)
      :login (login mesg channel)
      :logout (logout channel)
      :close (close channel)
      :mark (mark mesg channel)
      (info "no handler for: " (:dissoc mesg :value))
      )
    (catch Exception e
      (error e e))))
