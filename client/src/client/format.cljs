(ns client.format
  (:import [goog.i18n DateTimeFormat]))

(defn short-date [date]
  (.format (DateTimeFormat. "MMM dd hh:mm ") (js/Date. date)))

(defn fmt [date]
  (try 
    (condp instance? date
      js/Date (short-date date)
      date)
    (catch js/Object e
      (.error js/console e))))
