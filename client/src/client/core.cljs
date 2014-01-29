(ns client.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :refer [put! <! chan]]
            [cljs.reader :refer [read-string]]
            [clojure.browser.net :as net]
            [clojure.browser.event :as gevent]
            [clojure.browser.repl :as repl]
            [rohm.core :as rohm :include-macros true]
            [sablono.core :as html :refer [html] :include-macros true]
            [client.format :refer [fmt]]
            ))

(def app-state (atom {:user {} ;{:name "harry"}
                      :folders {:by-name {"INBOX" {
                                                   :messages {}}
                                          }
                                :current "INBOX"
                                }
                      :loading false }))

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
    (if-not (-> reading :flags :seen)
      (rohm/put-msg :mark [:messages] {:value {:seen true}}))
    new-val))

(defn up-message [{:keys [by-id marker] :as old-val} mesg]
  (if-let [reading (get-in old-val [:reading])]
    (assoc-in old-val [:reading] nil)
    (do
      (rohm/effect-messages [{:type :init :topic [:messages]}])
      old-val)))

(defn mark-messages [{:keys [by-id marker] :as old-val} mesg]
  (let [[flag bool] (first (:value mesg))
        msg-nums (if-let [reading (get-in old-val [:reading])]
                   (vector (:id reading))
                   (keys (filter #(:selected (val %)) (get-in old-val [:by-id]))))]
    (rohm/effect-messages [{:type :mark :topic [:messages] :value {:msg-nums msg-nums :flag {flag bool}}}])
    (update-in old-val [:by-id] 
               #(reduce (fn [by-id id]
                          (update-in by-id [id :flags] (if bool conj disj) flag)) % msg-nums))))

(defn init-messages [old-val mesg]
  (let [new-by-id (:value mesg)
        msg-num (apply max (keys new-by-id))]
    (rohm/put-msg :update [:folders :by-name "INBOX" :messages :marker] {:value msg-num})
    (rohm/put-msg :set [:loading] true)
    (apply sorted-map (flatten (seq new-by-id)))))

(defn merge-messages [old-val mesg]
  (let [new-by-id (:value mesg)]
    (reduce (fn [by-id [id m]] 
                          (update-in by-id [id] merge m))
                       old-val new-by-id)))

#_(defn delete-messages [old-val mesg]
  (let [msg-nums (if-let [reading (get-in old-val [:reading])]
                   (vector (:id reading))
                   (keys (filter #(:selected (val %)) (get-in old-val [:by-id]))))]
    (rohm/effect-messages [{:type :delete :topic [:messages] :value msg-nums}])
    ; TODO reset :reading or remove selection
    (update-in old-val [:by-id] 
               #(reduce (partial mark-message conj :deleted) % (keys %)))))

#_(defn undelete-messages [old-val mesg]
  (let [msg-nums (if-let [reading (get-in old-val [:reading])]
                   (vector (:id reading))
                   (keys (filter #(:selected (val %)) (get-in old-val [:by-id]))))]
    (rohm/effect-messages [{:type :undelete :topic [:messages] :value msg-nums}])
    ; TODO reset :reading or remove selection
    (update-in old-val [:by-id] 
               #(reduce (partial mark-message disj :deleted) % (keys %)))))

(defn login-user [old-val mesg]
  (rohm/put-msg :set [:loading] true)
  (rohm/effect-messages [{:type :login :topic [:user] :value (:value mesg)}])
  old-val)

(def routes [
             [:move [:messages] move-marker]
             [:update [:**] (fn [o m] (rohm/put-msg :set [:loading] false) (:value m))]
             [:set [:**] (fn [o m] (:value m))]
             [:select [:messages] select-message]
             [:read [:messages] read-message]
             [:up [:messages] up-message]
             [:mark [:messages] mark-messages]
             [:init [:messages :by-id] init-messages]
             [:merge [:messages :by-id] merge-messages]
             [:login [:user] login-user]
             ;[:delete [:messages] delete-messages]
             ;[:undelete [:messages] undelete-messages]
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
         [:button {:className (str "btn btn-sm" (if (= index current) " active"))}
          index]]))))

(defn folder-box [folders]
  (om/component
    (html
      [:div.folder-box
       (rohm/map-of folder-elem (:by-name folders) {:opts {:current (:current folders)}})])))

(defn unread [is-read v]
  (if-not is-read
    [:strong v]
    v))

(defn message-elem [mesg owner]
  (let [is-read (contains? (:flags mesg) :seen)]
    (om/component
      (html
        [:tr {:className (str "message" 
                              (if is-read " read")
                              (if (:selected mesg) " selected")
                              (if (:deleted (:flags mesg)) " deleted")
                              )}
         [:td {:className (if (:marker mesg) " marked")} " "]
         [:td.selector [:input {:type "checkbox" :checked (:selected mesg)}]]
         ;[:td (:id mesg)]
         [:td (unread is-read (or (:personal (:sender mesg)) (:address (:sender mesg))))]
         ;[:td (count (:content mesg))]
         [:td (unread is-read (:subject mesg))]
         [:td.sent (unread is-read (fmt (:sent mesg)))]
         ;[:td (pr-str mesg)]
         ]))))

(defn message-list [{:keys [by-id marker]} owner]
  (om/component
    (html
      [:div.message-list ;marker
       [:table.table 
        [:tbody 
         (for [[i mesg] (reverse by-id)]
           (om/build message-elem mesg 
             {:key :id 
              :fn (partial (fn [mid m] (if (= mid (:id m)) (assoc m :marker true) m)) marker)})) ]]])))

(defn message-view [messages]
  (let [marker (:marker messages)
        reading (get-in messages [:by-id marker])
        {:keys [subject sender sent content flags] :as mesg} reading
        content-type (:type content)
        content (:content content)]
    (om/component
      (html
        [:div.thread
         [:h4 subject]
         [:div.mesg
          [:div.info
           [:span [:strong (or (-> sender :personal) (-> sender :address))]]
           [:span.pull-right (-> sent fmt)]]
          (if (= content-type :html)
            [:div.content.html.body {:dangerouslySetInnerHTML #js {"__html" content}}]
            [:div.content nil (if content [:pre content] "Loading...")])
          ]]))))

(defn login-screen [app owner]
  (letfn[(signin [e]
           (rohm/put-msg :login [:user] {:value (rohm/extract-refs owner)})
           false)]
    (om/component
      (html 
        [:div.login (str (:msg (:user app)))
         [:form.form-horizontal {:role "form"}
          [:div.form-group
           [:label.col-sm-2.control-label {:htmlFor "inputUsername"} "Username"]
           [:div.col-sm-4
            [:input#inputUsername.form-control {:ref "username" :type "text" :placeholder "Username" 
                                                :defaultValue "info@empanda.net"}]]]
          [:div.form-group
           [:label.col-sm-2.control-label {:htmlFor "inputPassword"} "Password"]
           [:div.col-sm-4
            [:input#inputPassword.form-control {:ref "password" :type "Password" :placeholder "Password" :autoFocus true }]]]
          [:div.form-group
           [:label.col-sm-2.control-label {:htmlFor "inputServer"} "Server"]
           [:div.col-sm-4
            [:input#inputServer.form-control {:ref "server" :type "Server" :placeholder "Server" 
                                              :defaultValue "mail.empanda.net"}]]]
          [:div.form-group
           [:div.col-sm-offset-2.col-sm-4
            [:button.btn.btn-default {:onClick signin} "Sign in"]]]]]))))

(def j-key 106)
(def k-key 107)
(def u-key 117)
(def x-key 120)
(def U-key 85)
(def I-key 73)
(def enter-key 13)
(def hash-key 35)
(def amp-key 64)

(defn key-press [e]
  (let [curr-fold (get-in @app-state [:folders :current])]
    (condp =  (.-which e)
      j-key (rohm/put-msg :move [:folders :by-name curr-fold :messages] {:value :down})
      k-key (rohm/put-msg :move [:folders :by-name curr-fold :messages] {:value :up})
      x-key (rohm/put-msg :select [:folders :by-name curr-fold :messages])
      enter-key (rohm/put-msg :read [:folders :by-name curr-fold :messages])
      u-key (rohm/put-msg :up [:folders :by-name curr-fold :messages])
      U-key (rohm/put-msg :mark [:folders :by-name curr-fold :messages] {:value {:seen false}})
      I-key (rohm/put-msg :mark [:folders :by-name curr-fold :messages] {:value {:seen true}})
      hash-key (rohm/put-msg :mark [:folders :by-name curr-fold :messages] {:value {:deleted true}})
      amp-key (rohm/put-msg :mark [:folders :by-name curr-fold :messages] {:value {:deleted false}})
      nil
      #_(.info js/console "key-press " (.-which e)))))

(defn mail-view [app]
  (reify
    om/IDidMount
    (did-mount [_ _]
      (rohm/effect-messages [{:type :init :topic [:messages]}])
      (.addEventListener js/window "keypress" key-press))
    om/IRender 
    (render [_]
      (html
        [:div.row
         [:div.col-md-2
          (om/build folder-box (:folders app))]
         [:div.col-md-10
          (:marker app)
          (let [current (get-in app [:folders :current])
                folder (get-in app [:folders :by-name current])]
            (if-let [reading  (-> folder :messages :reading)]
              (om/build message-view (:messages folder))
              (om/build message-list (:messages folder) {:opts (:marker folder)})))]]))))

(defn logout []
  (.removeEventListener js/window "keypress" key-press)
  (rohm/put-msg :update [:messages] {:value {}})
  (rohm/effect-messages [{:type :logout :topic [:user]}]))

(defn client-box [app]
  (let [username (:name (.-value (:user app)))]
    (om/component
      (html
        [:div.container
         [:div.clientBox
          [:h3.pull-left "Hmail"] (if username [:div.pull-right username [:button.btn {:onClick logout} "logout"]])
          [:div.loading {:className (str "loading-" (:loading app))} "loading..."]
          [:div.clearfix]
          (if (empty? username)
            (om/build login-screen app)
            (om/build mail-view app))]]))))

(def ws-prot (if (re-find #"localhost" window.location.host) "ws" "wss"))
(def socket (new js/WebSocket (str ws-prot "://" window.location.host "/ws")))
(set! (.-onmessage socket) (fn [e]
                             (if-not (= (.-data e) "ping")
                               (try
                                 (let [res (read-string (.-data e))]
                                   (rohm/put-msg res))
                                 (catch js/Object er
                                   (.error js/console (pr-str "error " (.-data e))))))))

(if-not (re-find #"localhost" window.location.host)
  (js/setInterval #(.send socket "ping") 50000)) ; keep socket open on Heroku

(defn client-service [message input-queue]
  (if (= 1 (.-readyState socket))
    (.send socket (pr-str message))))
  
(defn client-app [app]
  (reify
    om/IWillMount
    (will-mount [this]
      (rohm/handle-messages app-state routes client-service)
      ;(rohm/put-msg :update [:folders :by-id "INBOX" :messages :marker] {:value (apply max (keys (:by-id (:messages app))))})
      #_(repl/connect "http://localhost:9000/repl"))
    om/IRender
    (render [_]
      (om/build client-box app))))

(om/root app-state client-app (.getElementById js/document "container"))
