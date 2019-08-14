(ns constraint-processing.user
  (:require [clojure.pprint :refer [pprint]]
            [constraint-processing.learner :as learner]
            [constraint-processing.core :as paths]
            [constraint-processing.sfa :as sfa]
            [clojure.java.shell :as sh]
            [constraint-processing.ranges :as ranges]
            [com.rpl.specter :as sp])
  (:import LearnLarge
           TacasParser))


(binding [learner/*parse-fn* #(LearnLarge/parse %)]
  (println "-----")
  (let [db (paths/load-db-from-prefix "learn-large-" 1)
        table (learner/learn-with-coastal db 2)
        learnt (learner/build-sfa table)]
    (sfa/sfa->img learnt)
    (pprint learnt)
    (pprint (meta learnt))
    (pprint (meta table))
    (println "-----")
    ))


(learner/refine-path [[87] [16]])
