(ns server.messages
  (:use [clojure.tools.logging])
  (:require [server.mail :as mail :refer [have-messages? get-content prefetch check-new flag-with get-folders]]
            [clojure.core.async :refer [go chan <! >! <!!]]
            [org.httpkit.server :refer [send!]]
            [server.images :refer [stop-images]]
            ))

(defmacro ws-send [channel & body]
  `(go (send! ~channel (pr-str (do ~@body)))))

;; message helpers
(defn messages-mesg [msgs folder & [init]]
  {:type (if init :init :merge) :topic [:folders :by-name folder :messages :by-id] :value msgs})

(defn fetch-content-for-messages [channel folder msgs]
  (let [msg-nums (keys msgs)]
    (doseq [msg-num msg-nums]
      (ws-send channel
               (let [content (get-content channel folder msg-num)
                     content (stop-images content)]
                 (messages-mesg {msg-num {:content content}} folder))))))

(defn fetch-folders [channel]
  (ws-send channel 
           (if-let [folders (get-folders channel)]
             {:type :merge :topic [:folders :by-name] :value folders})))

(defn prefetch-messages [channel folder n & [from]]
  (let [msgs (prefetch channel folder n 0 from)]
    (fetch-content-for-messages channel folder msgs)
    msgs))

(def prefetch-size 3)

(defn- folder-from-topic [topic]
  (nth topic 2))

;; handlers
(defn init-messages [topic channel]
  (let [folder (folder-from-topic topic)]
    (info "init-messages folder " folder)
    (if-not (have-messages? channel folder)
      (do
        (ws-send channel 
                 (let [msgs (prefetch-messages channel folder prefetch-size)]
                   (messages-mesg msgs folder :init)))
        [])
      (let [msgs (check-new channel folder)]
        (fetch-content-for-messages channel folder msgs)
        (messages-mesg msgs folder)))))

(defn fetch-more [mesg channel]
  (ws-send channel 
           (let [folder (folder-from-topic (:topic mesg))
                 lowest (:value mesg)
                 msgs (prefetch-messages channel folder prefetch-size lowest)]
             (messages-mesg msgs folder)))
  nil)
 
(defn login [mesg channel]
  (try 
    (let [value (:value mesg)
          {:keys [username password server]} value]
      (info "login " username server)
      (mail/login channel username password server)
      (fetch-folders channel)
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
        folder (folder-from-topic (:topic mesg))
        msgs (flag-with channel folder msg-nums flag)
        msgs (into {} (map (fn [[k v]] [k {:flags (:flags v)}]) msgs))]
    (messages-mesg msgs folder)))

(defn handle-message [{:keys [type topic] :as mesg} channel]
  (try
    ;(info "handle-message " mesg)
    (condp = type
      :init (init-messages topic channel)
      :login (login mesg channel)
      :logout (logout channel)
      :close (close channel)
      :mark (mark mesg channel)
      :fetch-more (fetch-more mesg channel)
      (info "no handler for: " (dissoc mesg :value))
      )
    (catch Exception e
      (error e e))))
