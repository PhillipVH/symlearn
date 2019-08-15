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

(binding [learner/*parse-fn* #(TacasParser/parse %)]
  (let [db (paths/load-db-from-prefix "tacas-parser-" 1)
        table (learner/learn-with-coastal-dynamic db 1)
        learnt (learner/build-sfa table)]
    (pprint learnt)
    (sfa/complete? learnt)
    (println (meta table))
    (println (meta learnt))
    (sfa/sfa->img learnt)))

(binding [learner/*parse-fn* #(LearnLarge/parse %)]
  (let [db (paths/load-db-from-prefix "learn-large-" 3)
        table (learner/learn-with-coastal-dynamic db 2)
        learnt (learner/build-sfa table)]
    (pprint table)
    (sfa/complete? learnt)
    (sfa/sfa->img learnt)))
