(ns symlearn.sfa
  (:require [clojure.java.shell :as sh]
            [taoensso.timbre :as log]
            [symlearn.coastal :as coastal]
            [symlearn.intervals :as intervals])
  (:import automata.sfa.SFA
           java.util.LinkedList
           theory.characters.CharPred
           com.google.common.collect.ImmutableList))

;; sfa creation

(defn constraint-set->fn
  "[Constraint] -> (Char -> Boolean)"
  [constraint-set]
  (let [assert->fn (fn [[op bound]]
                     (case op
                       ">=" #(>= % bound)
                       ">" #(> % bound)
                       "==" #(== % bound)
                       "!=" #(not= % bound)
                       "<" #(< % bound)
                       "<=" #(<= % bound)))
        assertions (map assert->fn constraint-set)]
    (fn [input]
      (let [results (map #(% input) assertions)]
        (not (some false? results))))))

(defn pop*
  "Safe version of `clojure.core/pop`; returns nil when `coll` is empty"
  [coll]
  (when (seq coll)
    (pop coll)))

(defn- compute-prefix-pairs
  [{:keys [S R]}]
  (let [s+r (merge S R)]
    (reduce (fn [prefix-pairs [path _ :as entry]]
              (let [prefixes  (filter (fn [[other-path _]]
                                        (= (.constraints path)
                                           (pop* (.constraints other-path))))
                                      s+r)]
                (conj prefix-pairs [entry :is-prefix-of prefixes])))
            []
            s+r)))

(defn compute-transitions
  "Table -> [Transition]"
  [table]
  (let [prefix-pairs (compute-prefix-pairs table)
        transitions
        (reduce (fn [transitions [[prefix-path prefix-row] _ follow]]
                  (if (seq? follow)
                    (conj transitions
                          (map (fn [[path row]]
                                 (let [guard (first (drop (.length prefix-path) (.constraints path)))]
                                   {:from prefix-row
                                    :guard guard
                                    :to row})) follow))
                    transitions))
                []
                prefix-pairs)]
    (flatten transitions)))

(defn initial-state
  "Table -> State"
  [{:keys [S]}]
  (let [[[_ row]] (filter (fn [[path _]] (= 0 (coastal/length path))) S)]
    row))

(defn states
  [{:keys [S]}]
  (set (vals S)))

(defn- state-map
  "Convert transitions from our internal representation (states are rows, guards are
  constraint sets) to the format required by Symbolic Automata (states are integers,
  guards are CharPreds)."
  [table]
  (let [states (states table)
        state-labels (map-indexed (fn [idx state]
                                    [state idx]) states)]
    (into {} state-labels)))

(defn final-states
  [table]
  (let [states (states table)
        state-map (state-map table)
        final-states (filter (fn [state] (first state)) states)]
    (map #(get state-map %) final-states)))

(defn- transition->SFAInputMove
  [{:keys [from to guard]}]
  (intervals/make-transition from to guard))

(defn make-sfa
  [table & [{:keys [minimize?]}]]
  (let [state-map (state-map table)
        transitions (set (compute-transitions table))
        relabeled (set (map (fn [{:keys [from guard to]}]
                              {:from (get state-map from)
                               :to (get state-map to)
                               :guard (intervals/constraint-set->CharPred guard)})
                            transitions))
        transitions (map transition->SFAInputMove relabeled)
        initial-state (get state-map (initial-state table))
        final-states (final-states table)
        sfa (SFA/MkSFA
             (doto (LinkedList.) (.addAll transitions))
             (int initial-state)
             (doto (LinkedList.) (.addAll (map int final-states)))
             intervals/solver
             false
             false)]
    (if minimize?
      (.minimize sfa intervals/solver)
      sfa)))

(defn show-dot
  [table]
  (let [sfa (make-sfa table {:minimize? true})]
    (log/info sfa)
    (.createDotFile ^SFA sfa  "aut" "")
    (sh/sh "dot" "-Tps" "aut.dot" "-o" "outfile.ps")
    (sh/sh "xdg-open" "outfile.ps")
    (sh/sh "rm" "outfile.ps")))

(defn show-sfa
  [^SFA sfa]
  (.createDotFile sfa  "aut" "")
  (sh/sh "dot" "-Tps" "aut.dot" "-o" "outfile.ps")
  (sh/sh "xdg-open" "outfile.ps"))

(defn make-sfa*
  [table]
  (let [sfa (make-sfa table)]
    (.minimize ^SFA sfa intervals/solver))) ;; TODO This can throw org.sat4j.specs.TimeoutException

(defn ce->pred
  [ce]
  (let [path-condition (coastal/constraints (coastal/query ce))]
    (map intervals/constraint-set->CharPred path-condition)))

(defn- predicate-transitions
  [table]
  (let [state-map (state-map table)
        transitions (set (compute-transitions table))
        relabeled (set (map (fn [{:keys [from guard to]}]
                              {:from (get state-map from)
                               :to (get state-map to)
                               :guard (intervals/constraint-set->CharPred guard)})
                            transitions))]
    relabeled))

(defn- intersection-of-transitions
  [transitions]
  (let [guards (map :guard transitions)
        empty-intersection (CharPred/of (ImmutableList/of))
        intersections (for [guard guards] ;; for every transition out of q_i
                        (flatten (map (fn [other-guard]
                                        (if (identical? guard other-guard)
                                          empty-intersection
                                          (intervals/intersection guard other-guard)))
                                      guards)))
        all (set (flatten intersections))]
    (reduce (fn [pred intersection]
              (intervals/union pred intersection))
            empty-intersection
            all)))

(defn deterministic?
  "Check for any outgoing transitions that have non-empty intersection."
  [table]
  (let [grouped-transitions (group-by :from (predicate-transitions table))
        empty-intersection (CharPred/of (ImmutableList/of))
        intersection-per-state (map (fn [[_ transitions]]
                                      (intersection-of-transitions transitions))
                                    grouped-transitions)
        non-empty (filter #(not= % empty-intersection) intersection-per-state)]
    (= 0 (count non-empty))))

(defn regex->sfa*
  "A safe version of `intervals/regex->sfa`, catching unsupported regex exceptions from
  the underlying parser, and timing out on parsers that take too long to construct."
  [target]
  (coastal/timeout 5000 #(try (intervals/regex->sfa target) (catch Exception _ ::unsupported-regex))))

(defn equivalent?
  [^SFA target ^SFA candidate]
  (.isEquivalentTo target candidate intervals/solver))
