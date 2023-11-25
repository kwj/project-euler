(ns project-euler.sol.p0035
  (:require
   [project-euler.lib.math :as math]
   [project-euler.lib.math.prime :as prime]))

(defn- circular-prime?
  [n]
  (let [k (math/num-of-digits n)
        d (long (math/pow 10 (dec k)))]
    (loop [n n
           cnt (dec k)
           result true]
      (if (or (zero? cnt) (not result))
        result
        (let [next-n (+ (quot n 10) (* (mod n 10) d))]
          (recur next-n (dec cnt) (prime/prime? next-n)))))))

(defn solve
  ([]
   (solve 1000000))
  ([limit]
   (->> (prime/primes limit)
        (filter circular-prime?)
        (count))))
