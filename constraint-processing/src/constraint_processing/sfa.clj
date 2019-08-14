(ns constraint-processing.sfa
  "Utilities to facilitate the creation, validation, and manipulation of SFAs."
  (:require [constraint-processing.core :as paths]
            [constraint-processing.ranges :as ranges]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]]
            [clojure.java.shell :as sh]
            [com.rpl.specter :as sp]))

(defn state-predicates
  "Returns a mapping from state number to predicates on transitions leading
  out of that state."
  [sfa]
  (reduce (fn [acc state]
            (assoc acc state (set (sp/select [:transitions state sp/ALL :input] sfa))))
          {}
          (:states sfa)))

(defn- predicates->transitions
  [preds state]
  (map (fn [pred]
         {:from state, :input pred, :to state, :artificial true}) preds))

(defn complete?
  "Returns true if sfa is complete (i.e. has transitions out of every state
  that cover the entire input domain)."
  [sfa]
  (let [total-range-per-state
        (reduce (fn [acc [state predicates]]
                  (assoc acc state (ranges/get-largest-range predicates)))
                {}
                (state-predicates sfa))
        nil-states (sp/select [sp/MAP-VALS (sp/pred nil?)] total-range-per-state)]
    (empty? nil-states)))

(defn complete
  [sfa]
  (let [state-predicates (state-predicates sfa)]
    (reduce (fn [sfa state]
              (update-in sfa [:transitions state]
                         #(vec (concat % (flatten (predicates->transitions (ranges/get-completing-preds (sp/select [state sp/ALL] state-predicates)) state))))))
            sfa
            (:states sfa))))

(defn sfa->dot
  "Given some SFA, generate the dot code that will draw
  the automaton."
  [sfa]
  (let [header "digraph finite_state_machine { \nrankdir=LR; size=\"8.5\"\n "
        final-state-style (str "node [ shape = doublecircle]; " (str/join "; " (:final-states sfa)) ";\n")
        initial-state-style (str "node [ shape = point ]; qi;\n")
        normal-state-style (str "node [ shape = circle ];\n")
        first-transition (str "qi -> " (:initial-state sfa) ";\n")
        transitions (reduce
                     (fn [trans-string {:keys [from to input]}]
                       (let [input (if (= (second input) Integer/MAX_VALUE) [(first input) "∞"] input)]
                         (str trans-string
                              "\n"
                              (if (nil? from) "nil" from)
                              " -> "
                              (if (nil? to) "nil" to)
                              " [ label = \"["
                              (first input) " "
                              (second input) "]\"];")))
                     ""
                     (apply concat (map second (filter map-entry? (:transitions sfa)))))
        footer "\n}"]
    (-> header
        (str final-state-style)
        (str initial-state-style)
        (str normal-state-style)
        (str first-transition)
        (str transitions)
        (str footer))))

(defn sfa->img
  "Take an SFA and generate an image from it, using `sfa->dot`."
  [sfa]
  (spit "tmp.dot" (sfa->dot sfa))
  (sh/sh "dot"
         (str "-Gdpi=" 300)
         (str "-T" "png")
         "tmp.dot"
         "-o"
         "tmp.png")
  (sh/sh "xdg-open" "tmp.png")
  (sh/sh "rm" "tmp.png")
  (sh/sh "rm" "tmp.dot"))

