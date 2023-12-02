(ns project-euler.sol.p0063
  (:require [clojure.math]))

;;;; n - 1 <= log10(m^n) < n    [m>0, n>0]
;;;;  --> n - 1 <= n * log10(m) < n
;;;;  --> m < 10
;;;; and
;;;;  --> (n - 1)/n <= log10(m)
;;;;  --> n/n - (n -1)/n >= 1 - log10(m)
;;;;  --> 1/n >= 1 - log10(m)
;;;;  --> 1/(1 - log10(m)) >= n
;;;;  --> n_{max} = floor(1/(1 - log10(m)))

(defn solve
  []
  (->> (range 1 10)
       (map #(int (clojure.math/floor (/ 1.0 (- 1.0 (clojure.math/log10 %))))))
       (apply +)))
