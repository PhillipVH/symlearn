(ns symlearn.intervals
  (:import org.apache.commons.lang3.tuple.ImmutablePair
           (com.google.common.collect ImmutableList
                                      Iterators)
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
  (generate-witness [this] "Return a character that satisfies the constraint in `this`.")
  (satisfied-by [this character] "True if `character` satifies the constraints in `this`."))

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
  (satisfied-by [this character] (.isSatisfiedBy this character))

  IInterval
  (left [this] (first (first (intervals this))))
  (right [this] (second (first (intervals this)))))

(defn make-interval
  "Return a CharPred over `bottom` to `top`."
  [^Character bottom ^Character top]
  (CharPred. bottom top))

(defprotocol ISFA
  "A protocol for SFAs over the domain of characters."
  (initial-state [this] "Return the initial state of `this`.")
  (final-states [this] "Return the set of final states of `this`.")
  (state-count [this] "Return the number of states in `this`.")
  (transitions-from [this state] "Return all the transitions in `this` from `state`.")
  (transitions-to [this state] "Return all the transitions in `this` to `state`."))

(extend-type SFA
  ISFA
  (initial-state [this] (.getInitialState this))
  (final-states [this] (.getFinalStates this))
  (state-count [this] (.stateCount this))
  (transitions-from [this state] (.getTransitionsFrom this ^int (int state)))
  (transitions-to [this state] (.getTransitionsTo this ^int (int state))))

(defprotocol ITransition
  "A protocol for transitions over the domain of characters."
  (from [this] "Return the state from which `this` transition originates.")
  (to [this] "Return the state to which `this` transition goes.")
  (guard [this] "Return the guard used by `this` to check if a transition should occur."))

(defn constraint->CharPred
  [[op bound]]
  (case op
    ">" (CharPred. (char (inc bound)) \uFFFF)
    ">=" (CharPred. (char bound) \uFFFF)
    "<" (CharPred. \u0000 (char (dec bound)))
    "<=" (CharPred. \u0000 (char bound))
    "==" (CharPred. (char bound))
    "!=" (negate (CharPred. (char bound)))))

(defn constraint-set->CharPred
  [constraints]
  (reduce (fn [predicate constraint]
            (intersection predicate (constraint->CharPred constraint)))
          (CharPred. \u0000 \uFFFF)
          constraints))

(extend-type SFAInputMove
  ITransition
  (from [this] (.from this))
  (to [this] (.to this))
  (guard [this] (.guard this)))

(defn make-transition
  "Return a transition that goes from `from` to `to` over the predicate `guard`."
  [from to ^CharPred guard]
  (SFAInputMove. (int from) (int to) guard))

(defn ^SFA regex->sfa
  "Returns an SFA that accepts the language described by `regex`."
  [regex]
  (let [nodes (binding [*out* (java.io.StringWriter.)] ;; get rid of the "string to be parsed" message
                (RegexParserProvider/parse ^"[Ljava.lang.String;" (into-array [regex])))
        root (.get nodes 0)
        sfa (RegexConverter/toSFA root solver)
        completed (SFA/mkTotal sfa solver 1000)]
    (.minimize completed solver)))

(defn sfa->java
  "Return the Java source code that represents a parser accepting the language
  described by `sfa`."
  [^SFA sfa, package-name, class-name]
  (let [java-src (StringBuilder.)]
    (doto java-src
      (.append "package ")
      (.append package-name)
      (.append ";\n")
      (.append "import za.ac.sun.cs.coastal.Symbolic;\n")
      (.append "public final class ")
      (.append class-name)
      (.append " {\n")
      (.append "\tpublic static void main(String[] args) {\n")
      (.append "\t\tboolean result = parse(new char[]{'a'});\n")
      (.append "\t}\n"))

    (doto java-src
      (.append "\tpublic static boolean parse(char[] A) {\n")
      (.append "\t\tint state = ")
      (.append (.getInitialState sfa))
      (.append ";\n")
      (.append "\t\tfor (int idx = 0; idx < A.length; idx++) {\n")
      (.append "\t\t\tchar current = A[idx];\n"))

    (let [states-iter (.iterator (.getStates sfa))]
      (while (.hasNext states-iter)
        (let [state (.next states-iter)]
          (doto java-src
            (.append "\t\t\tif (state == ")
            (.append state)
            (.append ") {\n"))

          (let [transitions-iter (.iterator (.getTransitionsFrom sfa ^Integer state))]
            (while (.hasNext transitions-iter)
              (let [transition (.next transitions-iter)
                    interval-size (.. transition guard intervals size)]
                (.append java-src "\t\t\t\tif (")
                (doseq [id (range 0 interval-size)]
                  (let [bound (.. transition guard intervals (get id))
                        left (.getLeft bound)
                        right (.getRight bound)]
                    (when (> id 0)
                      (.append java-src " || "))

                    (if (not (.equals left right))
                      (doto java-src
                        (.append "(current >= ")
                        (.append (if (nil? left)
                                   "Character.MIN_VALUE"
                                   (str "(char)" (int (.charValue left)))))
                        (.append " && current <= ")
                        (.append (if (nil? right)
                                   "Character.MAX_VALUE"
                                   (str "(char)" (int (.charValue right)))))
                        (.append ")"))

                      (doto java-src
                        (.append "(current == ")
                        (.append (if (nil? left)
                                   "Character.MIN_VALUE"
                                   (str "(char)" (int (.charValue left)))))
                        (.append ")")))))
                (doto java-src
                  (.append ") {\n")
                  (.append "\t\t\t\t\tstate = ")
                  (.append (.to transition))

                  (.append ";\n")
                  (.append "\t\t\t\t\tcontinue;\n")
                  (.append "\t\t\t\t}\n"))))))
        (.append java-src "\t\t\t}\n"))
      (doto java-src
        (.append "\t\t}\n")
        (.append "\t\tif ("))

      (let [states-iter (.iterator (.getFinalStates sfa))]
        (while (.hasNext states-iter)
          (let [final-state (.next states-iter)]
            (doto java-src
              (.append "(state == ")
              (.append final-state)
              (.append ") || ")))))
      (doto java-src
        (.append "false) { \n")
        (.append "\t\t\tSymbolic.mark(1);\n")
        (.append "\t\t\treturn true;\n")
        (.append "\t\t} else {\n")
        (.append "\t\t\tSymbolic.mark(0);\n")
        (.append "\t\t\treturn false;\n\t\t}\n")
        (.append "\t}")
        (.append "\n}\n")))

    (.toString java-src)))
