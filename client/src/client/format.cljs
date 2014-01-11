(ns client.format
  (:import [goog.i18n DateTimeFormat]))

(defn opening-hr [date]
  (str (nth date 0) 
       (if (nth date 1)
         "-"
         " ") (nth date 1) " " (nth date 2) "-" (nth date 3)))

(defn opening-hrs [date]
  (interpose ", " (map opening-hr date)))

(defn short-date [date]
  (.format (DateTimeFormat. "MMM dd hh:mm ") (js/Date. date)))

(defn fmt [date]
  (try 
    (condp instance? date
      js/Date (short-date date)
      om.core/MapCursor (str (fmt (:from date)) " - " (fmt (:till date)))
      cljs.core/List (opening-hrs date)
      date)
    (catch js/Object e
      (.error js/console e))))

(defn icon [cat-or-type]
  (if-let [ic
           (case cat-or-type
             "Music" "glyphicon-music"
             "venues" "glyphicon-home"
             "Restaurants" "glyphicon-cutlery"
             "features" "glyphicon-user"
             "Film" "glyphicon-film"
             "Films" "glyphicon-film"
             "events" "glyphicon-calendar"
             "Shopping" "glyphicon-shopping-cart"
             "Theatre" "glyphicon-sound-dolby"
             "TimeIn" "glyphicon-resize-small"
             "Art" "glyphicon-picture"
             "AroundTown" "glyphicon-refresh"
             "Bars&Pubs" "glyphicon-glass"
             "Travel" "glyphicon-plane"
             "Gay&Lesbian" "glyphicon-barcode"
             "Mind&Body" "glyphicon-heart-empty"
             "Adult" "glyphicon-remove"
             "Sport&Outdoor" "glyphicon-road"
             "Museums&Attractions" "glyphicon-tower"
             nil)]
    (str "glyphicon " ic)))
