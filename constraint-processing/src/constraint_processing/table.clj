(ns constraint-processing.table
  "Functions that relate to the creation and modification of
  the Observation Table.")

(defn make-table
  "Create an empty observation table."
  []
  {:S #{{:path [] :row []}}
   :R #{}
   :E [[]]})

(defn fill-entry
  ""
  [entry evidence]
  (assoc entry :row (into [] (check-membership (:path entry) evidence))))

(defn fill-entries
  [entries evidence]
  (reduce (fn [filled entry]
            (conj filled (fill-entry entry evidence)))
          #{}
          entries))
