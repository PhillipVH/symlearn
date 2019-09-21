(ns symlearn.demo
  (:gen-class)
  (:require [clojure.java.io :as io]
            [symlearn.coastal :as coastal]
            [symlearn.learner :as learner]
            [symlearn.table :as table]
            [symlearn.sfa :as sfa])
  (:import LearnLarge
           PaperExample))

(def parse-fn-for
  {"LearnLarge" #(LearnLarge/parse %)
   "PaperExample" #(PaperExample/parse %)})

(defn -main
  [& [config-name depth]]
  (println "before")
  (let [dse-engine (coastal/start-coastal! "PaperExample.xml")
        _ (println "Started coasted")
        _ (prn (.isAlive dse-engine))
        {:keys [table]} (learner/learn {:depth 1, :parse-fn #(PaperExample/parse %)})
        _ (println "did learn " table)
        hypothesis (table/table->sfa table)]
    (clojure.pprint/pprint hypothesis)
    (coastal/stop-coastal! dse-engine)
    #_(sfa/sfa->img hypothesis))
  #_(if (#{"PaperExample" "LearnLarge"} config-name)
    (let [dse-engine (coastal/start-coastal! (str config-name ".xml"))
          {:keys [table]} (learner/learn {:depth depth, :parse-fn (parse-fn-for config-name)})
          hypothesis (table/table->sfa table)]
      (coastal/stop-coastal! dse-engine)
      (clojure.pprint/pprint hypothesis)
      #_(sfa/sfa->img hypothesis))
    (binding [*out* *err*]
      (println config-name "does not exist in symlearn/resources"))))
