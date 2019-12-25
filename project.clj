(defproject symlearn "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [org.clojure/core.specs.alpha "0.2.44"]
                 [org.clojure/test.check "0.9.0"]
                 [com.taoensso/carmine "2.19.1"]
                 [com.taoensso/tufte "2.0.1"]
                 [cljstache "2.0.4"]
                 [com.rpl/specter "1.1.2"]
                 [com.taoensso/timbre "4.10.0"]

                 ;; symbolic automata
                 [cs.wisc.edu/Parsers "1.0"]
                 [cs.wisc.edu/SVPAlib "1.0"]
                 [SVPABenchmark/SVPABenchmark "0.0.1-SNAPSHOT"]
                 [org.sat4j/org.sat4j.core "2.3.1"]
                 [org.apache.commons/commons-lang3 "3.4"]
                 [com.google.guava/guava "18.0"]
                 [javacup/cup "11"]]
  :main ^:skip-aot symlearn.coastal
  :repositories {"local" "file:lib"}
  :java-source-paths ["src/main/java"]
  :target-path "target/%s"
  :resource-paths ["resources/"
                   "symbolicautomata/commons-lang3-3.4.jar"
                   "symbolicautomata/guava-18.0.jar"
                   "symbolicautomata/org.ow2.sat4j.core-2.3.4.jar"
                   "symbolicautomata/SVPAlib-1.0.jar"
                   "symbolicautomata/SVPABenchmark-0.0.1-SNAPSHOT.jar"
                   "symbolicautomata/Parsers-1.0.jar"
                   "symbolicautomata/javacup-11c.jar"]
  :profiles {:uberjar {:aot :all}})
