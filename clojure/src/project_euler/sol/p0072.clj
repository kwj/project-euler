(ns project-euler.sol.p0072
  (:require [project-euler.lib.math :as math]))

(def sum-phi
  (memoize (fn [n]
             (let [v (quot (* n (inc n)) 2)
                   sum-m (apply + (for [m (range 2 (inc (math/isqrt-long n)))]
                                    (sum-phi (quot n m))))
                   sum-d (apply + (for [d (range 1 (inc (quot n (+ (math/isqrt-long n) 1))))]
                                    (* (- (quot n d) (quot n (inc d))) (sum-phi d))))]
               (- v sum-m sum-d)))))

(defn solve
  ([]
   (solve 1000000))
  ([limit]
   (- (sum-phi limit) (sum-phi 1))))
