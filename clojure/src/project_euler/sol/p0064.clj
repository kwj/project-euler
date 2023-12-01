(ns project-euler.sol.p0064
  (:require [project-euler.lib.math :as math]))

(defn- get-cont-fraction
  [n]
  (let [isqrt-n (math/isqrt-long n)
        stop-condition (* isqrt-n 2)]
    (if (= (* isqrt-n isqrt-n) n)
      [isqrt-n, []]
      (loop [b isqrt-n
             c 1
             rep []]
        (let [next-c (long (quot (- n (* b b)) c))
              a (quot (+ isqrt-n b) next-c)]
          (if (= a stop-condition)
            [isqrt-n (conj rep a)]
            (recur (- (* a next-c) b) next-c (conj rep a))))))))

(defn solve
  ([]
   (solve 10000))
  ([limit]
   (->> (range 1 (inc limit))
        (map get-cont-fraction)
        (filter #(odd? (count (second %))))
        (count))))
