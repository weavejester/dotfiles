{:user
 {:plugins [[lein-cljfmt "0.6.1" :exclusions [org.clojure/clojure]]
            [lein-ancient "0.6.15" :exclusions [org.clojure/clojure]]]}
 :repl
 {:dependencies [[org.clojure/tools.nrepl "0.2.12"]]
  :repl-options {:timeout 300000}}}
