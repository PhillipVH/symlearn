(ns symlearn.intervals
  (:import org.apache.commons.lang3.tuple.ImmutablePair
           com.google.common.collect.ImmutableList
           com.google.common.collect.Iterators
           theory.intervals.UnaryCharIntervalSolver
           theory.characters.CharPred
           benchmark.regexconverter.RegexConverter
           (automata.sfa SFA
                         SFAInputMove)
           (RegexParser RegexNode
                        RegexParserProvider)))

(set! *warn-on-reflection* true)

(def ^UnaryCharIntervalSolver solver
  "An instance of `UnaryCharIntervalSolver` shared by all instances of `Pred`."
  (UnaryCharIntervalSolver.))

(defprotocol ISolvable
  "A protocol for character predicates solvable by `UnaryCharIntervalSolver`."
  (union [this pred] "Return the union of `this` and `pred`.")
  (intersection [this pred] "Return the intersection of `this` and `pred`.")
  (negate [this] "Return the negation of this `this`.")
  (equivalent? [this that] "Return true if `this` and `that` are equivalent.")
  (generate-witness [this] "Return a character that satisfies the constraint in `this`."))

(defprotocol IInterval
  "A protocol for character predicates expressed as intervals."
  (left [this] "Return the left bound of `this`.")
  (right [this] "Return the right bound of `this`."))

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

(extend-type CharPred
  ISolvable
  (union [this that] (.MkOr solver this that))
  (intersection [this that] (.MkAnd solver this that))
  (negate [this] (.MkNot solver this))
  (equivalent? [this that] (.AreEquivalent solver this that))
  (generate-witness [this] (.generateWitness solver this))

  IInterval
  (left [this] (first (first (intervals this))))
  (right [this] (second (first (intervals this)))))

(defn make-interval
  "Return a CharPred over `bottom` to `top`."
  [^Character bottom ^Character top]
  (CharPred. bottom top))

(defn regex->sfa
(defn ^SFA regex->sfa
  "Returns an SFA that accepts the language described by `regex`."
  [regex]
  (let [nodes (RegexParserProvider/parse ^"[Ljava.lang.String;" (into-array [regex]))
        root (.get nodes 0)
        sfa (RegexConverter/toSFA root solver)]
    (.determinize sfa solver)))

(comment

  (print (regex->sfa "a|(b|c)?"))
  (let [our-sfa (regex->sfa "a|(b|c)?")]
    (println our-sfa))

  (intervals (union (make-interval \a \g) (make-interval \z \z)))
  pred
  (left (make-interval \a \g))

  (intervals (negate (make-interval \a \g)))

  )


