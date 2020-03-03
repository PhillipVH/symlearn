(ns symlearn.time)

;; time utilities

(defn ms->m
  [ms]
  (/ ms 60000))

(defn m->ms
  "Returns `m` minutes in milliseconds."
  [m]
  (* 1000 60 m))

(defn ms-to-timeout
  "Returns the number of milliseconds left between `now` and `minutes-limit`."
  [start now minutes-limit]
  (let [diff (- now start)
        minutes (float (/ diff 60000))]
    (if (>= minutes minutes-limit)
      0
      (m->ms (- minutes-limit minutes)))))
