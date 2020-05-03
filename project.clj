(defproject symlearn "0.1.0-SNAPSHOT"
  :description "Learn Symbolic Finite Automata from Java Parsers using Concolic Execution and Active Learning."
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.0"]

                 ;; testing
                 [org.clojure/core.specs.alpha "0.2.44"]
                 [org.clojure/test.check "0.9.0"]

                 ;; redis
                 [com.taoensso/carmine "2.19.1"]

                 ;; profiling
                 [com.taoensso/tufte "2.0.1"]

                 ;; code generation
                 [cljstache "2.0.4"]

                 ;; logging
                 [com.taoensso/timbre "4.10.0"]

                 ;; graph library
                 [aysylu/loom "1.0.2"]

                 ;; utilities
                 [com.rpl/specter "1.1.2"]
                 [aero "1.1.5"]
                 [clj-glob "1.0.0"]

                 ;; symbolic automata
                 [cs.wisc.edu/Parsers "1.0"]
                 [cs.wisc.edu/SVPAlib "1.0"]
                 [SVPABenchmark/SVPABenchmark "0.0.1-SNAPSHOT"]
                 [org.sat4j/org.sat4j.core "2.3.1"]
                 [org.apache.commons/commons-lang3 "3.4"]
                 [com.google.guava/guava "18.0"]
                 [javacup/cup "11"]]
  :main ^:skip-aot symlearn.coastal-lite
  :repositories {"local" "file:lib"}
  :target-path "target/%s"
  :resource-paths ["resources/"
                   "results/"]
  :jvm-opts ["-Xms4g" "-Xmx12g"]
  :profiles {:uberjar {:aot :all}})
