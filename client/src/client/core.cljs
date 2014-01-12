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
                                ["Inbox" "Other"]
                                :current "Inbox"}
                      :messages {
                                 :by-id {255
                                         {:id 255
                                          :subject "subject"
                                          :sender {:personal "harry"
                                                   :address "harry@empanda.net"}
                                          :content "<html>
                                                   <body>
                                                   Hi Harry
                                                   <p><p>
                                                   I wonder if you could do us a favour, the milk containers you have for us, would you be able to drop them to Brian and Shirleyanne's place as we will be seeing them this evening at the folk music concert in Cobargo. You and Lena should come too if you are not to tired after picking beans.
                                          
                                          the musician is Andy Irvine an Irish folk singer
                                                   </p><p>
                                          
                                          Cheers. </body></html>"
                                           :sent #inst "2014-01-12T13:01:52.000-00:00"
                                          :isRead false}
                                         254
                                         {:id 254
                                          :subject "hoi"
                                          :sender {:personal "Lena"
                                                   :address "harry@empanda.net"}
                                          :content "werken"
                                          :sent #inst "2014-01-09T08:42:52.000-00:00"
                                          :isRead true}
                                         253
                                         {:id 253
                                          :subject "Milk containers"
                                          :sender {:personal "Sam"
                                                   :address "harry@empanda.net"}
                                          :content "Hi Harry
                                                   <br/>
                                                   I wonder if you could do us a favour, the milk containers you have for us, would you be able to drop them to Brian and Shirleyanne's place as we will be seeing them this evening at the folk music concert in Cobargo. You and Lena should come too if you are not to tired after picking beans.
                                          
                                          the musician is Andy Irvine an Irish folk singer
                                          
                                          Cheers."
                                          :sent #inst "2014-01-08T08:42:52.000-00:00"
                                          :isRead true}
                                         }
                                 }}))

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
        new-marker (if-not (contains? (set ids) new-marker) marker new-marker)
        new-val (assoc-in old-val [:marker] new-marker)]
    (if (:reading old-val)
      (rohm/put-msg :read [:messages]))
    new-val))

(defn select-message [{:keys [by-id marker] :as old-val} mesg]
  (update-in old-val [:by-id marker :selected] not))

(defn read-message [{:keys [by-id marker] :as old-val} mesg]
  (let [reading (get-in old-val [:by-id marker])
        new-val (assoc-in old-val [:reading] reading)]
    (assoc-in new-val [:by-id marker :isRead] true)))

(defn up-message [{:keys [by-id marker] :as old-val} mesg]
  (if-let [reading (get-in old-val [:reading])]
    (assoc-in old-val [:reading] nil)
    (do
      (rohm/effect-messages [{:type :init :topic [:messages]}])
      old-val)))

(defn mark-messages [{:keys [by-id marker] :as old-val} mesg]
  (let [read-val (case (:value mesg)
                   :unread false
                   :read true)]
    (update-in old-val [:by-id] 
               (fn [by-id]
                 (into {} (map 
                            (fn [[id m]] 
                              [id (if (:selected m) 
                                    (assoc m :isRead read-val)
                                    m)]) by-id))))))

(def routes [
             [:move [:messages] move-marker]
             [:update [:messages :marker] (fn [o m] (:value m))]
             [:select [:messages] select-message]
             [:read [:messages] read-message]
             [:up [:messages] up-message]
             [:mark [:messages] mark-messages]
             ])

;; Pedestal style effect functions
;; return collection of messages for the effect queue

(defn fail [ev]
    (.error js/console "fail: " ev))

(defn folder-elem [folders owner {:keys [index current]}]
  (let [folder (get folders index)]
    (om/component
      (html
        [:div.folder
         [:button {:className (str "btn btn-sm" (if (= folder current) " active"))}
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
      (html
        [:tr {:className (str "message" 
                              (if is-read " read")
                              (if (:selected mesg) " selected")
                              )}
         [:td {:className (if (:marker mesg) " marked")} " "]
         [:td.selector [:input {:type "checkbox" :checked (:selected mesg)}]]
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
           (om/build message-elem mesg 
             {:key :id 
              :fn (partial (fn [mid m] (if (= mid (:id m)) (assoc m :marker true) m)) marker)})) ]]])))

(defn message-view [{:keys [subject sender sent content] :as mesg}]
  (om/component
    (html
      [:div.thread
       [:h4 subject]
       [:div.mesg
        [:div.info
         [:span [:strong (-> sender :personal)]]
         [:span.pull-right (-> sent fmt)]]
        [:div.content {:dangerouslySetInnerHTML #js {"__html" content}}  ]]])))

(defn client-box [app]
  (om/component
    (html
      [:div.container
       [:div.clientBox
        [:h3 "Hmail"]
        [:div.row
         [:div.col-md-2
          (om/build folder-box (:folders app))]
         [:div.col-md-10
          (:marker app)
          (if-let [reading (-> app :messages :reading)]
            (om/build message-view reading)
            (om/build message-list (:messages app) {:opts (:marker app)}))
          ]]]])))

(def socket (new js/WebSocket (str "ws://" window.location.host "/ws")))
(set! (.-onmessage socket) (fn [e]
                             (if-not (= (.-data e) "ping")
                               (let [res (cljs.reader/read-string (.-data e))]
                                 (.info js/console (pr-str res))
                                 ;(rohm/put-msg res)
                                 ))))
(js/setInterval #(.send socket "ping") 50000)

(defn client-service [message input-queue]
  (if (= 1 (.-readyState socket))
    (.send socket (pr-str message))))
  
(def j-key 106)
(def k-key 107)
(def u-key 117)
(def x-key 120)
(def U-key 85)
(def I-key 73)
(def enter-key 13)

(defn key-press [e]
  (condp =  (.-which e)
    j-key (rohm/put-msg :move [:messages] {:value :down})
    k-key (rohm/put-msg :move [:messages] {:value :up})
    x-key (rohm/put-msg :select [:messages])
    enter-key (rohm/put-msg :read [:messages])
    u-key (rohm/put-msg :up [:messages])
    U-key (rohm/put-msg :mark [:messages] {:value :unread})
    I-key (rohm/put-msg :mark [:messages] {:value :read})
    (.info js/console "key-press " (.-which e))))

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
      (rohm/effect-messages [{:type :init :topic [:messages]}])
      (.addEventListener js/window "keypress" key-press))
    om/IRender
    (render [_]
      (om/build client-box app))))

(om/root app-state client-app (.getElementById js/document "container"))
