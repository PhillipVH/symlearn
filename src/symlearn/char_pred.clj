(ns symlearn.char-pred
  (:import org.apache.commons.lang3.tuple.ImmutablePair
           com.google.common.collect.ImmutableList
           theory.intervals.UnaryCharIntervalSolver
           theory.characters.CharPred))

(set! *warn-on-reflection* true)

(def ^UnaryCharIntervalSolver solver
  "An instance of `UnaryCharIntervalSolver` shared by all instances of `Pred`."
  (UnaryCharIntervalSolver.))

(defprotocol Solvable
  "A protocol for character predicates solvable by `UnaryCharIntervalSolver`."
  (union [this pred] "Return the union of `this` and `pred`.")
  (intersection [this pred] "Return the intersection of `this` and `pred`.")
  (negate [this] "Return the negation of this `this`.")
  (equivalent? [this that] "Return true if `this` and `that` are equivalent."))

(defrecord Pred [^CharPred pred]
  Solvable
  (union [this that] (->Pred (.MkOr solver (:pred this) (:pred that))))
  (intersection [this that] (->Pred (.MkAnd solver (:pred this) (:pred that))))
  (negate [this] (->Pred (.MkNot solver (:pred this))))
  (equivalent? [this that] (.AreEquivalent solver (:pred this) (:pred that))))

(defn ^Pred make-pred
  "Return a CharPred over `bottom` to `top`."
  [^String bottom ^String top]
  (->Pred (CharPred. ^Character (.charAt bottom 0) ^Character (.charAt top 0))))

(comment

  (def consumer (reify java.util.function.Consumer
                  (accept [this t]
                    (println t))))

  (let [p1 (make-pred "a" "g")
        p2 (make-pred "z" "z")
        p1' (make-pred "a" "g")]
    (let [constraints #{}]
      )
    (println (equivalent? p1 p1'))
    (.forEach (.intervals ^CharPred (:pred (union p1 p2))) consumer))

  )



