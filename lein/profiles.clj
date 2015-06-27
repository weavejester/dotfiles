{:user
 {:dependencies [[org.clojure/tools.nrepl "0.2.10" :exclusions [org.clojure/clojure]]]
  :plugins [[cider/cider-nrepl "0.9.1"]
            [refactor-nrepl "1.1.0-SNAPSHOT"]
            [lein-cljfmt "0.1.10" :exclusions [org.clojure/clojure]]]
  :repl-options {:timeout 300000}}}
