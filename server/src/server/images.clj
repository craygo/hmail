(ns server.images
  (:use [net.cgrand.enlive-html]
  ))

(defn stop-images [content]
  (if (= (:type content) :html)
    (let [html-str (:content content)
          nodes (with-open [fl (java.io.StringReader. html-str)]
                  (html-resource fl))]
      (assoc content :content (apply str (emit* (at nodes [:img] (remove-attr :src))))))
    content))
