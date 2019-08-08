(ns constraint-processing.user
  (:require [clojure.pprint :refer [pprint]]
            [constraint-processing.learner :as learner]
            [constraint-processing.core :as paths]
            [constraint-processing.sfa :as sfa]
            [clojure.java.shell :as sh]))


(let [db (paths/load-db-from-prefix "tacas-parser-" #_"learn-large-" 2)
      obs-table (learner/learn-with-coastal db)
      learnt (learner/build-sfa obs-table)]
  (pprint learnt)
  #_(sfa/sfa->img learnt)
  #_(sfa/complete? learnt))

(def the-sfa
  {:transitions
   {3
    [{:from 3, :input [99 99], :to 2}
     {:from 3, :input [25 30], :to 0}
     {:from 3, :input [31 98], :to 3}
     {:from 3, :input [100 2147483647], :to 3}
     {:from 3, :input [0 24], :to 3}],
    0 [{:from 0, :input [0 10], :to 1}],
    1
    [{:from 1, :input [21 2147483647], :to 1}
     {:from 1, :input [0 20], :to 3}]},
   :initial-state 1,
   :final-states #{1},
   :states #{0 1 3 2}})

(defn complete
  "Returns sfa with transitions added to make all the states complete."
  [sfa]
  (sfa/state-predicates sfa)
  )

(complete the-sfa)

