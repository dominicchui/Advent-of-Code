(defproject advent-of-code "advent-of-code"
            :description "Solutions to advent of code in clojure"
            :license {}
            :dependencies [[org.clojure/clojure "1.10.0"]
                           [ysera "2.0.1"]]
            :target-path "target/%s"
            :profiles {:uberjar {:aot :all}})