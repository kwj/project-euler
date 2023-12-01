(ns project-euler.sol.p0027
  (:require
   [project-euler.lib.math.prime :as prime]))

(defn- count-consec-times
  [a b]
  (loop [cnt 1]
    (if (prime/simple-prime? (+ (* cnt (+ cnt a)) b))
      (recur (inc cnt))
      cnt)))

(defn solve
  []
  (let [p-lst (prime/primes 2000)
        pairs-ab (for [b (filter #(< % 1000) (rest p-lst))
                       a (map #(- % b 1) p-lst)
                       :when (< (abs a) 1000)]
                   [[a b] (count-consec-times a b)])]
    (let [[x y] (->> (apply max-key second pairs-ab)
                     (first))]
      (* x y))))

