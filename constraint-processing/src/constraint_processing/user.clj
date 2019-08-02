(ns constraint-processing.user
  (:require [clojure.string :as str]
            [clojure.pprint :refer [pprint]]
            [clojure.java.shell :as sh]
            [constraint-processing.learner :as learner]
            [constraint-processing.core :as paths]))


(def tacas-files
  "A list of files containing path conditions extracted from `TacasParser`."
  ["tacas-parser-1"
   "tacas-parser-2"
   ;; "tacas-parser-3"
   ;; "tacas-parser-4"
   ;; "tacas-parser-5"
   ;; "tacas-parser-6"
   ;; "tacas-parser-7"
   ])





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
                         (str trans-string "\n" from " -> " (if (nil? to) "nil" to) " [ label = \"[" (first input) " " (second input) "]\"];")))
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


(let [db (paths/load-files tacas-files)
      sfa (learner/learn-with-coastal db)]
  #_(assoc-in sfa [:transitions ])
  #_(pprint (learner/make-queries sfa 3))
  (sfa->img sfa)
  #_(sfa->img))


(pprint (learner/make-queries sfa 4))


{:accepted true, :path [[0 20] [99 99] [25 30] [0 10]]}



(def sfa {:transitions
          {2
           [{:from 2, :input [21 2147483647], :to 2}
            {:from 2, :input [0 20], :to 0}],
           0
           [{:from 0, :input [100 2147483647], :to 0}
            {:from 0, :input [31 98], :to 0}
            {:from 0, :input [99 99], :to 0}
            {:from 0, :input [25 30], :to 1}
            {:from 0, :input [0 24], :to 0}],
           1 [{:from 1, :input [0 10], :to 2}
              {:from 1, :input [11 Integer/MAX_VALUE, :to 0]}]},
          :initial-state 2,
          :final-states #{2}})
