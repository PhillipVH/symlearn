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

(defn ^SFA regex->sfa
  "Returns an SFA that accepts the language described by `regex`."
  [regex]
  (let [nodes (RegexParserProvider/parse ^"[Ljava.lang.String;" (into-array [regex]))
        root (.get nodes 0)
        sfa (RegexConverter/toSFA root solver)
        determinized (.determinize sfa solver)
        completed (SFA/mkTotal sfa solver 1000)]
    completed))

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

          (let [transitions-iter (.iterator (.getTransitionsFrom sfa state))]
            (while (.hasNext transitions-iter)
              (let [transition (.next transitions-iter)
                    interval-size (.. transition guard intervals size)]
                (.append java-src "\t\t\t\tif (")
                (doseq [id (range 0 interval-size)]
                  (let [bound (.. transition guard intervals (get 0))
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

(comment

  (spit "Regex.java" (sfa->java (regex->sfa "(ab|b)+") "regex" "Regex"))
  (println (regex->sfa "a+"))

    (let [our-sfa (regex->sfa "a|(b|c)?")]
      (println (SFA/mkTotal our-sfa solver 1000 )))

  (intervals (union (make-interval \a \g) (make-interval \z \z)))
  pred
  (left (make-interval \a \g))

  (intervals (negate (make-interval \a \g)))

  )


