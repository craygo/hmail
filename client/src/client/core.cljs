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

(def app-state (atom {:user {}
                      :folders {:list
                                ["Inbox" "Other"]
                                :current "Inbox"}
                      :messages {}
                      :loading false
                      }))

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
    (update-in new-val [:by-id marker :flags] conj :seen)))

(defn up-message [{:keys [by-id marker] :as old-val} mesg]
  (if-let [reading (get-in old-val [:reading])]
    (assoc-in old-val [:reading] nil)
    (do
      (rohm/effect-messages [{:type :init :topic [:messages]}])
      old-val)))

(defn mark-messages [{:keys [by-id marker] :as old-val} mesg]
  (let [f (case (:value mesg)
            :unread disj
            :read conj)]
    (update-in old-val [:by-id] 
               #(reduce (fn [by-id id] 
                          (if (:selected (get by-id id))
                            (update-in by-id [id :flags] f :seen)
                            by-id))
                        % (keys %)))))

(defn init-messages [old-val mesg]
  (let [new-by-id (:value mesg)
        msg-num (apply max (keys new-by-id))]
    (rohm/put-msg :update [:messages :marker] {:value msg-num})
    (rohm/put-msg :set [:loading] true)
    (assoc-in old-val [:by-id] (apply sorted-map (flatten (seq new-by-id))))))

(defn merge-messages [old-val mesg]
  (let [new-by-id (:value mesg)]
    (update-in old-val [:by-id] 
               #(reduce (fn [by-id [id m]] 
                          (update-in by-id [id] merge m))
                       % new-by-id))))

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
             [:init [:messages] init-messages]
             [:merge [:messages] merge-messages]
             [:login [:user] login-user]
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
  (let [is-read (contains? (:flags mesg) :seen)]
    (om/component
      (html
        [:tr {:className (str "message" 
                              (if is-read " read")
                              (if (:selected mesg) " selected")
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
                                                :autoFocus true :defaultValue ""}]]]
          [:div.form-group
           [:label.col-sm-2.control-label {:htmlFor "inputPassword"} "Password"]
           [:div.col-sm-4
            [:input#inputPassword.form-control {:ref "password" :type "Password" :placeholder "Password"}]]]
          [:div.form-group
           [:label.col-sm-2.control-label {:htmlFor "inputServer"} "Server"]
           [:div.col-sm-4
            [:input#inputServer.form-control {:ref "server" :type "Server" :placeholder "Server" 
                                              :defaultValue ""}]]]
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

(defn key-press [e]
  (condp =  (.-which e)
    j-key (rohm/put-msg :move [:messages] {:value :down})
    k-key (rohm/put-msg :move [:messages] {:value :up})
    x-key (rohm/put-msg :select [:messages])
    enter-key (rohm/put-msg :read [:messages])
    u-key (rohm/put-msg :up [:messages])
    U-key (rohm/put-msg :mark [:messages] {:value :unread})
    I-key (rohm/put-msg :mark [:messages] {:value :read})
    nil
    #_(.info js/console "key-press " (.-which e))))

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
          (if-let [reading (-> app :messages :reading)]
            (om/build message-view (:messages app))
            (om/build message-list (:messages app) {:opts (:marker app)}))
          ]]))))

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
      (rohm/put-msg :update [:messages :marker] {:value (apply max (keys (:by-id (:messages app))))})
      #_(repl/connect "http://localhost:9000/repl"))
    om/IRender
    (render [_]
      (om/build client-box app))))

(om/root app-state client-app (.getElementById js/document "container"))
