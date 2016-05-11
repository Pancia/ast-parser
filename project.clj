(defproject synfnetic "0.1.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/clojurescript "1.7.228"]
                 [org.omcljs/om "1.0.0-alpha32" :scope "provided"]
                 [navis/untangled-spec "0.3.6" :scope "test"]]

  :plugins [[com.jakemccrary/lein-test-refresh "0.14.0"]]

  :test-refresh {:report untangled-spec.reporters.terminal/untangled-report
                 :with-repl true
                 :changes-only true}

  :source-paths ["src"]
  :test-paths ["specs"]
  :clean-targets ^{:protect false} ["resources/public/js/compiled"]

  :cljsbuild {:builds [{:id           "test"
                        :source-paths ["specs" "src"]
                        :figwheel     true
                        :compiler     {:main                 synfnetic.spec-main
                                       :output-to            "resources/public/js/specs/specs.js"
                                       :output-dir           "resources/public/js/compiled/specs"
                                       :asset-path           "js/compiled/specs"
                                       :optimizations        :none}}]}

  :profiles {:dev {:source-paths ["dev" "src"]
                   :dependencies [[binaryage/devtools "0.5.2"]
                                  [com.cemerick/piggieback "0.2.1"]
                                  [figwheel-sidecar "0.5.0-6" :exclusions [joda-time clj-time]]
                                  [org.clojure/tools.nrepl "0.2.12"]]
                   :repl-options {:init-ns user
                                  :nrepl-middleware [cemerick.piggieback/wrap-cljs-repl]}}})
