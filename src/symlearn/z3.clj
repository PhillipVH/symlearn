(ns symlearn.z3
  (:refer-clojure :exclude [assert])
  (:require [clojure.string :as str]
            [clojure.java.shell :as sh]
            [taoensso.tufte :as tufte]))

(set! *warn-on-reflection* true)

(defn- assert
  [[op bound]]
  (let [negation (= "!=" op)
        op (if (or (= "!=" op)
                   (= "==" op)) "=" op)]
    (if negation
      (format "(assert (not (%s a %s)))\n" op bound)
      (format "(assert (%s a %s))\n" op bound))))

(defn- solve*
  "Runs the z3 solver with the constraints specified in `ctx`. Returns
  a witness if one exists, and `nil` if the constraints are unsat."
  [assertions]
  (tufte/p
   ::solve
   (let [prog (str "(declare-const a Int)\n" (str/join assertions) "(check-sat)\n(get-model)\n")
         z3-output (:out (sh/sh "z3" "-in" :in prog))
         negative (re-seq #"- " z3-output)
         witness (read-string (re-find #"\d+" z3-output))]
     (when (str/starts-with? z3-output "sat")
       (if negative (- witness) witness)))))

(def solve (memoize solve*))

(defn witness
  "Return a witness that satisfies a list of constraint sets."
  [constraint-sets]
  (let [assertions (map (fn [constraints] (map assert constraints)) constraint-sets)
        witness (map solve assertions)]
    witness))
