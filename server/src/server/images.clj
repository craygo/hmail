(ns server.images
  (:use [net.cgrand.enlive-html]
        [clojure.tools.logging])
  (:import [java.io BufferedReader StringReader]))

(defn split-on-curly [line]
  (map #(str % "}") (clojure.string/split line #"}")))

(defn safe-body [node]
  (update-in node [:content]
             (fn [s]
               (let [s (first s)]
                 (with-open [rdr1 (StringReader. s)
                             rdr (BufferedReader. rdr1)]
                   (let [lines (-> rdr line-seq)
                         lines (if (> (count lines) 1)
                                 lines
                                 (split-on-curly (first lines)))]
                     (->> lines
                          (map #(if (re-find #"\{" %)
                                  (if (or (.startsWith % "body")
                                          (.startsWith % "html"))
                                    (str ".content." %)
                                    (str ".content " %))
                                  %))
                          (clojure.string/join "\n"))))))))

(defn stop-images [content]
  (if (= (:type content) :html)
    (let [html-str (:content content)
          nodes (with-open [fl (java.io.StringReader. html-str)]
                  (html-resource fl))]
      (assoc content :content (apply str (emit* (at nodes 
                                                    [:img] (set-attr :src "/img/blocked.gif")
                                                    [:style] safe-body
                                                    )))))
    content))
