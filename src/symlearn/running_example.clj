(ns symlearn.running-example
  (:require [clojure.string :as str]
            [clojure.pprint :refer [pprint]]
            [taoensso.tufte :as profile]
            [symlearn.coastal :as coastal]
            [symlearn.table :as table]
            [symlearn.evaluation :as evaluation]
            [symlearn.intervals :as intervals]
            [symlearn.time :as time]
            [symlearn.gtestr :as gtestr]
            [symlearn.sfa :as sfa]))

;; define the running example
(def running-example-regex ".*[a-zA-Z0-9_][^a-zA-Z0-9_][0-9][^0-9].*" )
(def running-example-sfa (intervals/regex->sfa running-example-regex))

;; natural path condition repr
(defn run [word]
  (let [{:keys [constraints]} (coastal/query word)
        raw (coastal/refine-string word)]
    [(str/join "" (map intervals/constraint-set->CharPred constraints))
     (second raw)]))

;; learn a target and report profiling information
(comment

  (sfa/show-sfa running-example-sfa)

  (coastal/install-parser! running-example-regex)
  (def results
    (profile/profile
     {}
     (evaluation/evaluate!
      {:target running-example-regex
       :depth 2
       :timeout-ms (time/m->ms 10)
       :oracle :perfect})))
  (println (dissoc results :table))

  (def trimmed (table/shrink-table (:table results) 100))
  (pprint (:table results))
  )

;; grammar stuff

(comment
  (gtestr/sf)
  (println (gtestr/regex->gtestr running-example-regex))
  )
