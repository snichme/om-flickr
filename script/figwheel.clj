(require '[figwheel-sidecar.repl :as r]
         '[figwheel-sidecar.repl-api :as ra])

(ra/start-figwheel!
 {:figwheel-options {:ring-handler 'om-starter.server/app
                     :css-dirs ["resources/public/css"]
                     :open-file-command "ec"}
  :build-ids ["dev"]
  :all-builds
  [{:id "dev"
    :figwheel true
    :source-paths ["src"]
    :compiler {:main 'om-starter.core
               :asset-path "js"
               :output-to "resources/public/js/main.js"
               :output-dir "resources/public/js"
               :verbose true}}]})

(ra/cljs-repl)
