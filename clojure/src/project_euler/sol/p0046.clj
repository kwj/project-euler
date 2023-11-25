(ns project-euler.sol.p0046
  (:require
   [project-euler.lib.math :as math]
   [project-euler.lib.math.prime :as prime]))

(defn- twice-square?
  [n]
  (and (even? n)
       (= (math/pow (math/isqrt (quot n 2)) 2) (quot n 2))))

(defn solve
  []
  (loop [x 35
         odd-primes '(31 29 23 19 17 13 11 7 5 3)]
    (cond
      (prime/prime? x) (recur (+ x 2) (cons x odd-primes))
      (some #(twice-square? (- x %)) odd-primes) (recur (+ x 2) odd-primes)
      :else x)))
