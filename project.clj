(defproject om-starter "0.1.0-SNAPSHOT"
  :description "My first Om program!"
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/clojurescript "1.7.170"]
                 [org.omcljs/om "1.0.0-alpha31"]
                 [figwheel-sidecar "0.5.0-SNAPSHOT" :scope "provided"]
                 [bidi "1.20.3"]
                 [ring/ring "1.4.0"]
                 [clj-http "2.1.0"]
                 [cheshire "5.5.0"]
                 [com.cognitect/transit-clj "0.8.281"]
                 [com.cognitect/transit-cljs "0.8.225"]
                 [cljs-http "0.1.30" :exclusions
                  [org.clojure/clojure org.clojure/clojurescript
                   com.cognitect/transit-cljs]]])
