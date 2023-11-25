(ns project-euler.sol.p0066
  (:require [project-euler.lib.math :as math]))

(defn- get-cont-fraction
  [n]
  (let [isqrt-n (math/isqrt n)
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

(defn- get-numerator
  [a0 rep-lst]
  (first (reduce (fn [[x1 x2] a] [(+' (*' a x1) x2) x1]) [a0 1] rep-lst)))

(defn solve
  ([]
   (solve 1000))
  ([limit]
   (->> (range 1 (inc limit))
        (map #(vector % (get-cont-fraction %)))
        (filter #(not (zero? (count (second (second %))))))
        (map (fn [[i [a0 lst]]]
               (if (even? (count lst))
                 [i (get-numerator a0 (drop-last lst))]
                 [i (get-numerator a0 (drop-last (concat lst lst)))])))
        (apply max-key #(second %))
        (first))))
