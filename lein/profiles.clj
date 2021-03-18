{:user
 {:dependencies [[hashp "0.1.1"]]
  ;:injections [(require 'hashp.core)]
  :plugins [[lein-cljfmt "0.6.6" :exclusions [org.clojure/clojure]]
            [lein-ancient "0.6.15" :exclusions [org.clojure/clojure]]
            [lein-eftest "0.5.9"]
            [lein-try "0.4.3"]]}}
