(defproject sudoku "1.0.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [midje "1.8.3"]
                 [midje-grader "0.1.0-SNAPSHOT"]]
  :profiles {:dev {:plugins [[lein-midje "3.1.1"]]}})
