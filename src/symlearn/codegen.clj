(ns symlearn.codegen
  (:require [clojure.java.shell :as sh]
            [clojure.string :as str]
            [taoensso.timbre :as log]
            [cljstache.core :refer [render-resource]]
            [symlearn.intervals :as intervals])
  (:import (java.util Iterator Collection)
           java.io.File
           (automata.sfa SFA SFAInputMove)
           org.apache.commons.lang3.tuple.ImmutablePair
           theory.characters.CharPred))

;; code generation and compilation

(defn compile-parsers!
  "Compile the parsers installed in the Coastal system."
  []
  (sh/with-sh-dir "coastal"
    (sh/sh "./gradlew" "clean"))
  (let [coastal-dir (File. "coastal")
        args (into-array ["./gradlew" "compileJava" "--no-daemon"])
        builder (ProcessBuilder. ^"[Ljava.lang.String;" args)]
    (.directory builder coastal-dir)
    (let [compiler (.start builder)]
      (while (.isAlive compiler))
      ::ok)))

(defn compile-equivalence-oracle!
  []
  (sh/with-sh-dir "eqv-coastal-new"
    ;; compile coastal
    (let [clean-log (:out (sh/sh "./gradlew" "clean"))]
      (log/info clean-log))
    (let [gradle-log (:out (sh/sh "./gradlew" "--build-cache" "compileJava" "installDist" "-x" "test"))]
      (log/info gradle-log))

    ;; install coastal runner
    (log/info (:out (sh/sh "cp" "-r" "build/install/coastal/" "build/classes/java/main/")))))

(defn- mk-input
  [n]
  (str/join \, (take n (repeatedly (constantly " '.' ")))))

(defn sfa->java
  "Return the Java source code that represents a parser accepting the language
  described by `sfa`."
  [^SFA sfa fn-name]
  (let [java-src (StringBuilder.)]
    (doto java-src
      (.append (str "\tpublic static boolean " fn-name " (char[] A) {\n"))
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

          (let [transitions-iter ^Iterator (.iterator ^Collection (.getTransitionsFrom ^SFA sfa ^Integer state))]
            (while (.hasNext transitions-iter)
              (let [transition ^SFAInputMove (.next transitions-iter)
                    zi-guard ^CharPred (.guard transition)
                    zi-intervals (.intervals zi-guard)
                    interval-size (.size zi-intervals)]
                (.append java-src "\t\t\t\tif (")
                (doseq [id (range 0 interval-size)]
                  (let [zi-guard ^CharPred (.guard transition)
                        zi-intervals (.intervals zi-guard)
                        ;; bound (.. transition guard intervals (get id))
                        bound (.get zi-intervals id)
                        left (.getLeft ^ImmutablePair bound)
                        right (.getRight ^ImmutablePair bound)]
                    (when (> id 0)
                      (.append java-src " || "))

                    (if (not (.equals left right))
                      (doto java-src
                        (.append "(current >= ")
                        (.append (if (nil? left)
                                   "Character.MIN_VALUE"
                                   (str "(char)" (int (.charValue ^Character left)))))
                        (.append " && current <= ")
                        (.append (if (nil? right)
                                   "Character.MAX_VALUE"
                                   (str "(char)" (int (.charValue ^Character right)))))
                        (.append ")"))

                      (doto java-src
                        (.append "(current == ")
                        (.append (if (nil? left)
                                   "Character.MIN_VALUE"
                                   (str "(char)" (int (.charValue ^Character left)))))
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
        (.append "\t\t\treturn true;\n")
        (.append "\t\t} else {\n")
        (.append "\t\t\treturn false;\n\t\t}\n")
        (.append "\t}")))

    (.toString java-src)))

(defn mk-equivalence-oracle
  [^SFA candidate-sfa target depth]
  (render-resource "templates/Example.java" {:target-fn (sfa->java (intervals/regex->sfa target) "target")
                                             :candidate-fn (sfa->java candidate-sfa "candidate")
                                             :input (mk-input depth)}))

(defn install-equivalence-oracle!
  [^SFA candidate target depth]
  (spit "eqv-coastal-new/src/main/java/learning/Example.java"
        (mk-equivalence-oracle candidate target depth))
  (log/info "Compiling Equivalence Oracle:" {:target target, :depth depth})
  (compile-equivalence-oracle!))


