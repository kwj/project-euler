(ns project-euler.sol.p0027
  (:require
   [project-euler.lib.math.prime :as prime]))

(defn- count-consec-times
  [a b]
  (->> (iterate inc 0)
       (drop-while #(prime/prime? (+ (* % %) (* % a) b)))
       (first)
       (dec)))

(defn solve
  []
  (let [p-lst (prime/primes 2000)
        pairs-ab (for [b (filter #(< % 1000) (rest p-lst))
                       a (map #(- % b 1) p-lst)
                       :when (< (abs a) 1000)]
                   [a b])]
    (->> pairs-ab
         (map (fn [[a b]] [(* a b) (count-consec-times a b)]))
         (apply max-key second)
         (first))))

