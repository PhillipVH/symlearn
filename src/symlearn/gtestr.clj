(ns symlearn.gtestr
  (:require [clojure.string :as str]
            [clojure.zip :as zip]
            [clojure.walk :as walk]
            [loom.graph :refer [digraph predecessors edges nodes src dest]]
            [loom.label :refer [add-labeled-edges]]
            [loom.io :refer [view]]
            [clojure.java.shell :as sh]
            [org.satta.glob :as glob]
            [symlearn.intervals :as i :refer [negate union]]
            [symlearn.coastal :as coastal])
  (:import (automata.sfa SFA SFAInputMove)))

(set! *warn-on-reflection* true)

;; conversion of a symbolic state machine into a gtestr grammar

(def ascii-printable (i/make-interval \u0020 \u007E))

(defn printable [interval]
  (i/intersection ascii-printable interval))

(defn char-range [start end]
  (map char (range (int start) (inc (int end)))))

(defn escape-char [char]
  (case char
    \' "\\'"
    \\ "\\\\"
    char))

(defn transition->rule [^SFAInputMove transition]
  (let [from (.from transition)
        to (.to transition)
        guard (printable (.guard transition))
        expanded-range (char-range (i/left guard)
                                   (i/right guard))]
    (for [symbol expanded-range]
      (format "s%s :- '%s', s%s." from (escape-char symbol) to))))

(defn epsilon-rules [^SFA sfa]
  (map #(format "s%s :- epsilon." %)
       (.getFinalStates sfa)))

(defn sfa->gtestr [^SFA sfa]
  (let [rules (flatten (map transition->rule (.getTransitions sfa)))
        epsilon-rules (epsilon-rules sfa)
        header "gtestr :- assert(writer:token_separator(_,_,'') :- true).\n"
        grammar (str/join \newline (concat rules epsilon-rules))]
    (str header grammar)))

(defn regex->gtestr [regex]
  (let [machine (i/regex->sfa regex)]
    (sfa->gtestr machine)))

(defn save-grammar [grammar path]
  (spit path grammar))

;; compile gtestr

(defn compile-gtestr! []
  (sh/with-sh-dir "gtestr"
    (sh/sh "make")))

;; generate a test suite from a grammar file

(defn options->seq [options]
  (let [nested-seqs (map (fn [[k v]] [(str "--" (name k))
                                      (if-not (= true v) (str v))])
                         options)]
    (filter (complement nil?)
            (flatten nested-seqs))))

(defn generate-test-suite [grammar-file options]
  (sh/sh "cp" grammar-file "gtestr/")
  (sh/with-sh-dir "gtestr"
    (apply sh/sh (concat ["./gtestr" grammar-file]
                         (options->seq options)))))

(defn clean []
  (sh/with-sh-dir "gtestr"
    (sh/sh "rm" "-rf" "pos" "neg" "rand")))

(def negative-opts {:dir "neg"
                    :dl-mutation true
                    :rule true
                    :negative-only true
                    :rule-mutation true
                    :nll true
                    :strict 3
                    :no-explanations true})

(def positive-opts {:dir "pos"
                    :full-cdrc true
                    :strict 3
                    :no-explanations true})

(def random-opts {:dir "rand"
                  :random-size 10
                  :random-depth 25
                  :strict 3
                  :no-explanations true})

;; run a test suite over an oracle

(defn run-test-suite [{:keys [name ^SFA membership-oracle ^SFA hypothesis]}]
  (let [tests (glob/glob (format "gtestr/%s/**/*.hypothesis" name))]
    (println "Running " (count tests))
    (reduce (fn [counter-examples test]
              (let [input (->> test
                               slurp
                               str/trim
                               .toCharArray
                               (map char))]
                (if (not= (.accepts hypothesis input i/solver)
                          (.accepts membership-oracle input i/solver))
                  (conj counter-examples (str/join input))
                 counter-examples)))
            nil
            tests)))

;; public equivalence API

(defn check-equivalence [{:keys [^SFA oracle ^SFA hypothesis]}]
  ;; delete old tests TODO cache
  (clean)

  ;; generate the grammar
  (save-grammar (sfa->gtestr hypothesis) "hypothesis.pl")

  ;; generate the test suites
  (doseq [opts [positive-opts negative-opts random-opts]]
    (println (generate-test-suite "hypothesis.pl" opts)))

  ;; run the test suites and report counter examples
  (set (apply concat
              (map #(run-test-suite {:name %
                                     :membership-oracle oracle
                                     :hypothesis hypothesis})
                   ["pos" "neg" "rand"]))))

;; coastal-lite

(defn init-coastal-lite [oracle]
  (coastal/install-parser! oracle))

(defn guards [word]
  (let [{:keys [constraints]} (coastal/query word)
        guards (map i/constraint-set->CharPred constraints)]
    guards))

(defn witness [guard]
  (if (seq? guard)
    (str/join (map i/left guard))
    (str (i/left guard))))

(defn mk-guard
  ([bound] (mk-guard bound bound))
  ([lower upper] (i/make-interval lower upper)))

(defn unicode []
  (mk-guard \u0000 \uffff))

(defn outgoing [prefix]
    (loop [explored (negate (unicode))
           outgoing-guards #{}]
      (if (= (unicode) explored)
        outgoing-guards
        (let [next-guard (->> explored
                              negate
                              witness
                              (str prefix)
                              guards
                              last)]
          (recur (union explored next-guard)
                 (conj outgoing-guards next-guard))))))

(defn expand-graph [graph prefix]
  (let [guards (outgoing prefix)]
    (reduce (fn [graph guard]
              (add-labeled-edges graph [prefix (str prefix (witness (printable guard)))] guard))
            graph
            guards)))

(defn initial-graph []
  (expand-graph (digraph) ""))

(defn frontier [graph]
  (let [edges (edges graph)
        edges+length (map (juxt (comp count dest) identity) edges)
        grouped-by (group-by first edges+length)
        frontier-index (apply max (keys grouped-by))
        frontier (get grouped-by frontier-index)
        nodes-to-expand (map (fn [[_ node]] (dest node)) frontier)]
    nodes-to-expand))

(defn naive-unroll [steps]
  (loop [graph (initial-graph)
         steps steps]
    (if (= 0 steps)
      (view graph)
      (let [graph' (reduce (fn [graph' prefix]
                             (expand-graph graph' prefix))
                           graph
                           (frontier graph))]
        (recur graph' (dec steps))))))
