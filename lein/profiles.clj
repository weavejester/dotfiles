{:user
 {:plugins [[lein-cljfmt "0.3.0" :exclusions [org.clojure/clojure]]
            [lein-ancient "0.6.8" :exclusions [org.clojure/clojure]]]}
 :repl
 {:plugins
  [[cider/cider-nrepl "0.11.0-SNAPSHOT"]
   [refactor-nrepl "2.0.0-SNAPSHOT"]]
  :dependencies [[org.clojure/tools.nrepl "0.2.12"]]
  :repl-options {:timeout 300000}}}
