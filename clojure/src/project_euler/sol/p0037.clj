(ns project-euler.sol.p0037
  (:require
   [project-euler.lib.math :as math]
   [project-euler.lib.math.prime :as prime]
   [project-euler.lib.util :as util]))

(defn- make-right-truncatable-primes
  []
  (letfn [(aux [result cands]
            (if (empty? cands)
              result
              (let [next-cands
                    (->> (util/cartesian-product cands [1 3 7 9])
                         (map (fn [[x y]] (+ (* 10 x) y)))
                         (filter #(prime/prime? %)))]
                (aux (concat result next-cands) next-cands))))]
    (->> (util/cartesian-product (aux [2 3 5 7] [2 3 5 7]) [3 7])
         (map (fn [[x y]] (+ (* 10 x) y)))
         (filter #(prime/prime? %)))))

(defn- left-truncatable-prime?
  [n]
  (loop [d (math/num-of-digits n)]
    (cond
      (zero? d) true
      (prime/prime? (mod n (math/pow 10 d))) (recur (dec d))
      :else false)))

(defn solve
  []
  (->> (make-right-truncatable-primes)
       (filter left-truncatable-prime?)
       (apply +)))

