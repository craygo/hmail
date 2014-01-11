(defproject client "0.1.0-SNAPSHOT"
  :description "FIXME: write this!"
  :url "http://example.com/FIXME"

  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/clojurescript "0.0-2138" :scope "provided"]
                 [org.clojure/core.async "0.1.267.0-0d7780-alpha" :scope "provided"]
                 [rohm "0.1.0-SNAPSHOT"]
                 [sablono "0.1.4-SNAPSHOT"]]

  :plugins [[lein-cljsbuild "1.0.1"]]

  :source-paths ["src"]

  :cljsbuild {
              :builds [{:id "dev"
                        :source-paths ["src"]
                        :compiler {
                                   :output-to "main.js"
                                   :output-dir "out"
                                   :optimizations :none
                                   :source-map true
                                   :externs ["om/externs/react.js"]}}
                       {:id "release"
                        :source-paths ["src"]
                        :compiler {
                                   :output-to "main.js"
                                   :optimizations :advanced
                                   :pretty-print false
                                   :output-wrapper false
                                   :preamble ["om/react.min.js"]
                                   :externs ["om/externs/react.js"]
                                   :closure-warnings
                                   {:non-standard-jsdoc :off}}}
                       ]}
  )
