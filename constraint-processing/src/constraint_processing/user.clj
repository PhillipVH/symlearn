(ns constraint-processing.user
  (:require [clojure.pprint :refer [pprint]]
            [constraint-processing.learner :as learner]
            [constraint-processing.core :as paths]
            [constraint-processing.sfa :as sfa]
            [clojure.java.shell :as sh]
            [taoensso.tufte :as tufte]
            [constraint-processing.ranges :as ranges]
            [com.rpl.specter :as sp]
            [clojure.set :as set])
  (:import LearnLarge
           TacasParser
           PaperExample))

;; TACAS -- errors at depth three
(tufte/profile {} (binding [learner/*parse-fn* #(TacasParser/parse %)]
   (let [db (paths/load-db-from-prefix "tacas-parser-" 1)
         table (learner/learn-with-coastal-dynamic db 3)
         learnt (learner/build-sfa table)]
     (pprint table)
     (sfa/sfa->img learnt))))

 ;; NARGA E a bit before five



;; Paper example -- works well!
(tufte/profile {} (binding [learner/*parse-fn* #(PaperExample/parse %)]
                    (let [db (paths/load-db-from-prefix "paper-example-" 1)
                          table (learner/learn-with-coastal-dynamic db 4)
                          learnt (learner/build-sfa table)]
                      (pprint table)
                      (sfa/sfa->img learnt))))

(def x
  {:S
   #{{:path [[51 100] [0 20]], :row [false false true]}
     {:path [], :row [true false true]}
     {:path [[51 100]], :row [false false false]}},
   :R
   #{{:path [[101 2147483647]], :row [true false true]}
     {:path [[0 50]], :row [true false true]}},
   :E [[] [:c 51 0] [:c 0]]})

(def x'
  {:S
   #{{:path [[51 100] [0 20]], :row [false  true]}
     {:path [], :row [true  true]}
     {:path [[51 100]], :row [false  false]}},
   :R
   #{{:path [[101 2147483647]], :row [true  true]}
     {:path [[0 50]], :row [true  true]}},
   :E [[]  [:c 0]]})

(def x''
  {:S
   #{{:path [[51 100] [0 20]], :row [false ]}
     {:path [], :row [true ]}
     {:path [[51 100]], :row [false]}},
   :R
   #{{:path [[101 2147483647]], :row [true ]}
     {:path [[0 50]], :row [true ]}},
   :E [[]  #_[:c 0]]})

(defn row-equivalent?
  "Return true if `evidence` does not expose a new row in `table`, false otherwise."
  [table evidence]
  (let [table-with-evidence (learner/add-evidence table evidence)
        old-s-rows (set (map :row (:S table)))
        old-r-rows (set (map :row (:R table)))
        old-rows (set/union old-s-rows old-r-rows)
        new-s-rows (set (map :row (:S table-with-evidence)))
        new-r-rows (set (map :row (:R table-with-evidence)))
        new-rows (set/union new-s-rows new-r-rows)]
    (= (count new-rows) (count old-rows))))

(binding [learner/*parse-fn* #(PaperExample/parse %)]
  (pprint (row-equivalent? x'' [:c 51 0])))


;; Learn Large -- gets very slow
(tufte/profile {} (binding [learner/*parse-fn* #(LearnLarge/parse %)]
   (let [db (paths/load-db-from-prefix "learn-large-" 1)
         table (learner/learn-with-coastal-dynamic db 6)
         learnt (learner/build-sfa table)]
     (pprint table)
     (sfa/sfa->img learnt))))

(defonce stats-accumulator (tufte/add-basic-println-handler! {:ns-pattern "*"}))
