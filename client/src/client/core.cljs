(ns client.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :refer [put! <! chan]]
            [client.utils :refer [guid]]
            [clojure.browser.net :as net]
            [clojure.browser.event :as gevent]
            [clojure.browser.repl :as repl]
            [rohm.core :as rohm :include-macros true]
            [sablono.core :as html :refer [html] :include-macros true]
            [client.format :refer [fmt]]
            ))

(def app-state (atom {:folders {:list
                                ["INBOX" "other"]
                                :current "INBOX"}
                      :messages {
                                 :by-id {255
                                         {:id 255
                                          :subject "subject"
                                          :sender {:personal "harry"
                                                   :address "harry@empanda.net"}
                                          :content "content"
                                          :sent #inst "2014-01-09T08:42:52.000-00:00"
                                          :isRead false}
                                         254
                                         {:id 254
                                          :subject "hoi"
                                          :sender {:personal "Lena"
                                                   :address "harry@empanda.net"}
                                          :content "werken"
                                          :sent #inst "2014-01-08T08:42:52.000-00:00"
                                          :isRead true}
                                         }
                                 :marker 0}}))

(defn valid? [state]
  (and true))

(set-validator! app-state valid?)

;; Pedestal style input message transformer functions
(defn move-marker [{:keys [by-id marker] :as old-val} mesg]
  (let [ids (keys by-id)
        lowest (apply min ids)
        highest (apply max ids)
        f (case (:value mesg) 
            :down dec
            :up inc)
        new-marker (f marker)
        new-marker (if-not (contains? (set ids) new-marker) marker new-marker)]
  (assoc-in old-val [:marker] new-marker)))

(defn select-message [{:keys [by-id marker] :as old-val} mesg]
  (update-in old-val [:by-id marker :selected] not))

(def routes [
             [:move [:messages] move-marker]
             [:update [:messages :marker] (fn [o m] (:value m))]
             [:select [:messages] select-message]
             ])

;; Pedestal style effect functions
;; return collection of messages for the effect queue

(defn fail [ev]
    (.error js/console "fail: " ev))

(defn folder-elem [folders owner {:keys [index current]}]
  (let [folder (get folders index)]
    (om/component
      (html
        [:div 
         [:button {:className (str "btn" (if (= folder current) " active"))}
          folder]]))))

(defn folder-box [folders]
  (om/component
    (html
      [:div.folder-box
       (rohm/list-of folder-elem (:list folders) {:opts {:current (:current folders)}})
       ])))

(defn unread [is-read v]
  (if-not is-read
    [:strong v]
    v))

(defn message-elem [mesg owner]
  (let [is-read (:isRead mesg)]
    (om/component
      ;(.info js/console (pr-str mesg ))
      (html
        [:tr {:className (str "message" 
                              (if is-read " read")
                              (if (:marker mesg) " marked")
                              (if (:selected mesg) " selected")
                              )}
         [:td [:input {:type "checkbox" :checked (:selected mesg)}]]
         [:td (unread is-read (:personal (:sender mesg)))]
         [:td (unread is-read (:subject mesg))]
         [:td (unread is-read (fmt (:sent mesg)))]
         ;[:td (pr-str mesg)]
         ]))))

(defn message-list [{:keys [by-id marker]} owner]
  (om/component
    (html
      [:div.message-list ;marker
       [:table.table 
        [:tbody 
         (for [[i mesg] by-id]
           ;(.info js/console (pr-str  i mesg))
           (om/build message-elem mesg 
             {:key :id 
              :fn (partial (fn [mid m] (if (= mid (:id m)) (assoc m :marker true) m)) marker)})) ]]])))

(defn client-box [app]
  (om/component
    (html
      [:div.container
       [:div.clientBox
        [:h1 "Hmail"]
        [:div.row
         [:div.col-md-2
          (om/build folder-box (:folders app))]
         [:div.col-md-10
          (:marker app)
          (om/build message-list (:messages app) {:opts (:marker app)})]]]])))

(defn client-service [message input-queue]
  (letfn [(server-res [ev]
            (let [res (js->clj (.getResponseJson (.-target ev)) :keywordize-keys true)
                  res (vec (map #(assoc % :id (guid)) res))]
              #_(rohm/put-msg :reset [:client] {:client res})))
          (get-from-server [url]
            (let [xhr (net/xhr-connection)]
              (gevent/listen xhr "success" server-res)
              (gevent/listen xhr "error" fail)
              (net/transmit xhr url)))]
    ; need some kind of routing here too
    #_(if (and (= (:type message) :read) (:url message))
      (get-from-server (:url message))
      (.warn js/console (pr-str "client-service: don't know " message)))))
  
(def j-key 106)
(def k-key 107)
(def x-key 120)
(def enter-key 13)

(defn key-press [e]
  (condp =  (.-which e)
    j-key (rohm/put-msg :move [:messages] {:value :down})
    k-key (rohm/put-msg :move [:messages] {:value :up})
    x-key (rohm/put-msg :select [:messages])
    (.info js/console (.-which e))))

(defn client-app [app]
  (reify
    om/IWillMount
    (will-mount [this]
      (let [{:keys [url poll-interval]} app]
        ;(rohm/effect-messages (get-server-clients-effect url))
        ;(js/setInterval #(rohm/effect-messages (get-server-clients-effect %)) poll-interval url)
        (rohm/handle-messages app-state routes client-service)
        (rohm/put-msg :update [:messages :marker] {:value (apply max (keys (:by-id (:messages app))))})
        #_(repl/connect "http://localhost:9000/repl")))
    om/IDidMount
    (did-mount [_ _]
      (.addEventListener js/window "keypress" key-press))
    om/IRender
    (render [_]
      (om/build client-box app))))

(om/root app-state client-app (.getElementById js/document "container"))
