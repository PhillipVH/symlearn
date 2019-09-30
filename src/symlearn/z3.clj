(ns symlearn.z3
  (:refer-clojure :exclude [assert])
  (:require [clojure.string :as str]
            [clojure.java.shell :as sh]
            [taoensso.tufte :as tufte]))

(defn make-context
  ""
  []
  "(declare-const a Int)\n")

(defn assert
  [ctx op bound]
  (let [negation (= "!=" op)
        op (if (or (= "!=" op)
                   (= "==" op)) "=" op)]
    (if negation
      (str ctx (format "(assert (not (%s a %s)))\n" op bound))
      (str ctx (format "(assert (%s a %s))\n" op bound)))))

(defn solve
  "Runs the z3 solver with the constraints specified in `ctx`. Returns
  a witness if one exists, and `nil` if the constraints are unsat."
  [ctx]
  (tufte/p
   ::solve
   (let [prog (str ctx "(check-sat)\n(get-model)\n")
         z3-output (:out (sh/sh "z3" "-in" :in prog))
         negative (re-seq #"- " z3-output)
         witness (read-string (re-find #"\d+" z3-output))]
     (when (str/starts-with? z3-output "sat")
       (if negative (- witness) witness)))))

;; TODO generate and store a witness when a PC is first loaded, as we expect to
;; use these witness strings a LOT.
