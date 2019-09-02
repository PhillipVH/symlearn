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
  (equivalent? [this that] "Return true if `this` and `that` are equivalent.")
  (generate-witness [this] "Return a character that satisfies the constraint in `this`."))

(extend-type CharPred
  Solvable
  (union [this that] (.MkOr solver this that))
  (intersection [this that] (.MkAnd solver this that))
  (negate [this] (.MkNot solver this))
  (equivalent? [this that] (.AreEquivalent solver this that))
  (generate-witness [this] (.generateWitness solver this)))

(defn make-pred
  "Return a CharPred over `bottom` to `top`."
  [^Character bottom ^Character top]
  (CharPred. bottom top))

(defn intervals
  "Returns a set of [left right] pairs constructed from `interval`."
  [^CharPred interval]
  (let [iter (.iterator (.intervals interval))]
    (loop [intervals #{}]
      (if-let [has-next? (.hasNext iter)]
        (let [next ^ImmutablePair (.next iter)
              left (.getLeft next)
              right (.getRight next)]
          (recur (conj intervals [left right])))
        intervals))))

(comment

  (def consumer (reify java.util.function.Consumer
                  (accept [this t]
                    (println t))))

  (let [p1 (make-pred \a \b)
        p2 (make-pred \z \z)
        p1' (make-pred \a \a)
        un (union p1 p2)
        thelst (.copyOf (.intervals (:pred un)))
        ]
    (println (equivalent? p1 p1'))
    (.forEach (.intervals ^CharPred (:pred (union p1 p2))) consumer))


  (intervals (union (make-pred \a \g) (make-pred \z \z)))

  )


