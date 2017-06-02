(defproject lttest "0.1.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.8.0"]
                [org.clojure/math.combinatorics "0.1.4"]
                [lein-light-nrepl "0.3.3"]]
  :repl-options {:nrepl-middleware [lighttable.nrepl.handler/lighttable-ops]})