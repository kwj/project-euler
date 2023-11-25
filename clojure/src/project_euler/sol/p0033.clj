(ns project-euler.sol.p0033
  (:require
   [project-euler.lib.math :as math]
   [project-euler.lib.util :as util]))

(defn solve
  []
  (let [[numerator denominator]
        (->> (util/combination 3 (range 1 10))
             (filter (fn [[a b c]] (= (* 9 a (- c b)) (* c (- b a)))))
             (reduce (fn [[n0 d0] [n1 d1 _]] [(* n0 n1) (* d0 d1)]) [1 1]))]
    (quot denominator (math/gcd numerator denominator))))

