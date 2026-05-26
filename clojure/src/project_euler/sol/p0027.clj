(ns project-euler.sol.p0027
  (:require
   [project-euler.lib.math.prime :as prime]))

(defn- count-consec-times
  [[a b]]
  (loop [cnt 1]
    (if (prime/simple-prime? (+ (* cnt (+ cnt a)) b))
      (recur (inc cnt))
      cnt)))

(defn solve
  []
  (let [p-lst (prime/primes 2000)]
    (->> (for [b (filter #(< % 1000) (rest p-lst))
               a (map #(- % b 1) p-lst)
               :while (< a 1000)]
           [a b])
         (apply max-key count-consec-times)
         (apply *))))
