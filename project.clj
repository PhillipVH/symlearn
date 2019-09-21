(defproject symlearn "demo"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [org.clojure/core.async "0.4.500"]
                 [com.taoensso/carmine "2.19.1"]
                 [com.taoensso/tufte "2.0.1"]
                 [com.rpl/specter "1.1.2"]]
  :main ^:skip-aot symlearn.demo
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
