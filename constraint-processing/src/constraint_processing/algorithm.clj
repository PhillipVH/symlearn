(ns constraint-processing.algorithm
  (:require [constraint-processing.learner :as learner]
            [clojure.pprint :refer [pprint]]))

(defn learn-2
  [db]
  (let [counter (atom 0)]
    (loop [table (learner/init-table (learner/make-table) db)]
      (swap! counter inc)
      (cond
        ;; First handle the case in which the table is not closed
        (not (learner/closed? table))
        (do
          (println "----" @counter "----")
          (println "Closed Table")
          (pprint table)
          (recur (learner/close table db)))

        ;; Do an equivalence query and handle the evidence prefix addition
        (map? (learner/run-all-from-db (learner/build-sfa table) db))
        (let [counter-example (:path (run-all-from-db (learner/build-sfa table) db))
              path (into [] (concat [:c] counter-example))
              table-with-ce (learner/process-ce table {:path counter-example})
              ;; evidences (make-evidences (drop 1 path))
              evidences (make-evidences (suffix-difference (drop 1 path) (longest-matching-prefix db (drop 1 path))))
              table-with-evidence (apply-evidences table-with-ce evidences)]
          (do
            (println "----" @counter "----")
            (println "Added Counter Example & Evidence (Forward)")
            (pprint counter-example)
            (safe-dot (build-sfa table-with-evidence) "tacas" @counter)
            ;; (pprint table-with-evidence)
            (recur table-with-evidence)))

        ;; Do a reverse equivalence check
        (map? (run-all-from-sfa (build-sfa table) (make-queries (build-sfa table) 3)))
        (let [counter-example (:path (run-all-from-sfa (build-sfa table) (make-queries (build-sfa table) 3)))
              ;; path (into [] (concat [:c] counter-example))
              ;; table-with-ce (process-ce table {:path counter-example})
              ;; evidences (make-evidences (drop 1 path))
              ;; evidences (make-evidences (suffix-difference (drop 1 path) (longest-matching-prefix db (drop 1 path))))
              ;; table-with-evidence (apply-evidences table-with-ce evidences)
              ]
          (do
            (println "----" @counter "----")
            (println "Added Counter Example & Evidence (Backward)")
            ;; (wcar* (car/set "refine" (str/join " " (map first counter-example)))) ;; simulates putting a PC in for processing by Coastal
            (pprint counter-example)
            ;; (while (not= 1 (wcar* (car/exists "refined")))
            ;;   (println "Waiting for refinement from Coastal...")
            ;;   (Thread/sleep 1000))
            (let [refined-counter (refine-path (str/join " " (map first counter-example)))
                  ;; refined-counter (read-string (wcar* (car/get "refined")))
                  refined-evidences (make-evidences (suffix-difference refined-counter (longest-matching-prefix db refined-counter)))
                  table-with-ce (process-ce table {:path refined-counter})
                  table-with-refined-ev (apply-evidences table-with-ce refined-evidences)]
              ;; (wcar* (car/del "refined"))
              (pprint refined-counter)

              (safe-dot (build-sfa table-with-refined-ev) "tacas" @counter)
              (pprint table-with-refined-ev)
              (recur table-with-refined-ev))))

        ;; Uh, are we done?
        :default
        (let [n-rows (count (set/union (:R table) (:S table)))
              n-cols (count (:E table))]
          (println "----" @counter "----")
          (println "Done!")
          (pprint table)
          (println "Number of rows in R: " (count (:R table)))
          (println "Number of rows in S: " (count (:S table)))
          (println "Number of columns in E: " (count (:E table)))
          (safe-dot (build-sfa table) "tacas" @counter)
          (build-sfa table))))))
