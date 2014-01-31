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
            [client.utils :refer [save-state load-state]]
            ))

(def initial-state {:user {} ;{:name "harry"}
                      :folders {:by-name {"INBOX" 
                                          {:messages {}}}
                                :current "INBOX"
                                }
                      :loading false })

(def app-state (atom initial-state))

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
      (rohm/put-msg :read (:topic mesg)))
    (if (= new-marker lowest)
      (rohm/effect-messages [{:type :fetch-more :topic (:topic mesg) :value lowest}]))
    new-val))

(defn select-message [{:keys [by-id marker] :as old-val} mesg]
  (update-in old-val [:by-id marker :selected] not))

(defn read-message [{:keys [by-id marker] :as old-val} mesg]
  (let [reading (get-in old-val [:by-id marker])
        new-val (assoc-in old-val [:reading] reading)]
    (if-not (-> reading :flags :seen)
      (rohm/put-msg :mark (:topic mesg) {:value {:seen true}}))
    new-val))

(defn get-current-folder []
  (get-in @app-state [:folders :current]))

(defn update-current-folder-messages [& [current]]
  (let [current (or current (get-current-folder))]
    (rohm/put-msg :set [:loading] true)
    (rohm/effect-messages [{:type :init :topic [:folders :by-name current :messages]}])))

(defn up-message [{:keys [by-id marker] :as old-val} mesg]
  (if-let [reading (get-in old-val [:reading])]
    (assoc-in old-val [:reading] nil)
    (do
      (update-current-folder-messages)
      old-val)))

(defn mark-messages [{:keys [by-id marker] :as old-val} mesg]
  (let [[flag bool] (first (:value mesg))
        msg-nums (if-let [reading (get-in old-val [:reading])]
                   (vector (:id reading))
                   (keys (filter #(:selected (val %)) (get-in old-val [:by-id]))))]
    (rohm/effect-messages [{:type :mark :topic (:topic mesg) :value {:msg-nums msg-nums :flag {flag bool}}}])
    (rohm/put-msg :move (:topic mesg) {:value :down})
    (update-in old-val [:by-id] 
               #(reduce (fn [by-id id]
                          (update-in by-id [id :flags] (if bool conj disj) flag)) % msg-nums))))

(defn set-marker-to-first []
  (let [folder (get-current-folder)
        msg-num (apply max (keys (get-in @app-state [:folders :by-name folder :messages :by-id])))]
    (rohm/put-msg :update [:folders :by-name folder :messages :marker] {:value msg-num})))

(defn init-messages [old-val mesg]
  (let [new-by-id (:value mesg)]
    (rohm/put-msg :set-marker [])
    (rohm/put-msg :set [:loading] true)
    (apply sorted-map (flatten (seq new-by-id)))))

(defn merge-messages [old-val mesg]
  (let [new-by-id (:value mesg)]
    (rohm/put-msg :set [:loading] false)
    (reduce (fn [by-id [id m]] 
                          (update-in by-id [id] merge m))
                       old-val new-by-id)))

(defn login-user [old-val mesg]
  (rohm/put-msg :set [:loading] true)
  (rohm/effect-messages [{:type :login :topic [:user] :value (:value mesg)}])
  old-val)

(defn switch-folder [old-val mesg]
  (update-current-folder-messages (:value mesg))
  (assoc-in old-val [:current] (:value mesg)))

(def routes [
             [:move [:**] move-marker]
             [:update [:**] (fn [o m] (rohm/put-msg :set [:loading] false) (:value m))]
             [:set [:**] (fn [o m] (:value m))]
             [:select [:**] select-message]
             [:read [:**] read-message]
             [:up [:**] up-message]
             [:mark [:**] mark-messages]
             [:init [:** :by-id] init-messages]
             [:merge [:** :by-id] merge-messages]
             [:login [:**] login-user]
             [:switch [:**] switch-folder]
             [:set-marker [:**] set-marker-to-first]
             ])

;; Pedestal style effect functions
;; return collection of messages for the effect queue

(defn fail [ev]
  (.error js/console "fail: " ev))

(defn folder-elem [folders owner folder-name]
  (let [current (:current folders)
        switch-folder (fn [e] (rohm/put-msg :switch [:folders] {:value folder-name}) false)]
    (om/component
      (html
        [:div.folder
         [:button {:className (str "btn btn-sm" (if (= folder-name current) " active")) :onClick switch-folder}
          folder-name]]))))

(defn folder-box [folders]
  (om/component
    (html
      [:div.folder-box
       (for [[folder-name _] (:by-name folders)]
         (om/build folder-elem folders {:opts folder-name}))])))

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
         (if (pos? (count by-id))
           (for [[i mesg] (reverse by-id)]
             (om/build message-elem mesg 
                       {:key :id 
                        :fn (partial (fn [mid m] (if (= mid (:id m)) (assoc m :marker true) m)) marker)})))]]])))

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
                                                :defaultValue "harry"}]]]
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
(def s-key 115)
(def l-key 108)

(defn key-press [e]
  (let [curr-fold (get-current-folder)]
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
      s-key (save-state app-state)
      l-key (load-state app-state)
      nil
      #_(.info js/console "key-press " (.-which e)))))

(defn mail-view [app]
  (reify
    om/IDidMount
    (did-mount [_ _]
      (update-current-folder-messages)
      (.addEventListener js/window "keypress" key-press))
    om/IRender 
    (render [_]
      (html
        [:div.row
         [:div.col-md-2
          (om/build folder-box (:folders app))]
         [:div.col-md-10
          (let [current (get-in app [:folders :current])
                folder (get-in app [:folders :by-name current])]
            (if-let [reading  (-> folder :messages :reading)]
              (om/build message-view (:messages folder))
              (om/build message-list (:messages folder) {:opts (:marker folder)})))]]))))

(defn logout []
  (.removeEventListener js/window "keypress" key-press)
  (rohm/effect-messages [{:type :logout :topic [:user]}])
  (rohm/put-msg :update [] {:value initial-state}))

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

(defn- local-dev? []
  (re-find #"localhost" window.location.host))

(def ws-prot (if (local-dev?) "ws" "wss"))
(def socket (new js/WebSocket (str ws-prot "://" window.location.host "/ws")))
(set! (.-onmessage socket) (fn [e]
                             (if-not (or (= (.-data e) "ping") (= (.-data e) "nil"))
                               (try
                                 (let [res (read-string (.-data e))]
                                   (rohm/put-msg res))
                                 (catch js/Object er
                                   (.error js/console (pr-str "error reading data " (.-data e))))))))

(if-not (local-dev?)
  (js/setInterval #(.send socket "ping") 50000)) ; keep socket open on Heroku

(defn client-service [message input-queue]
  (if (= 1 (.-readyState socket))
    (.send socket (pr-str message))))
  
(defn client-app [app]
  (reify
    om/IWillMount
    (will-mount [this]
      (rohm/handle-messages app-state routes client-service)
      ;(.addEventListener js/window "keypress" key-press) ; only for dev
      #_(repl/connect "http://localhost:9000/repl"))
    om/IRender
    (render [_]
      (om/build client-box app))))

(om/root app-state client-app (.getElementById js/document "container"))
