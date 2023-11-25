;;; Note
;;;   This implementation cuts corners on processing for looping numbers.
;;;   It therefore doesn't work well for all chains.

(ns project-euler.sol.p0074
  (:require [project-euler.lib.util :as util]))

(defn- next-number
  [n]
  (let [fact-tbl [1 1 2 6 24 120 720 5040 40320 362880]]
    (if (zero? n)
      (get fact-tbl 0)
      (loop [n n acc 0]
        (if (zero? n)
          acc
          (recur (quot n 10) (long (+ acc (get fact-tbl (mod n 10))))))))))

(defn- find-chain
  [d]
  (let [n (util/undigits d)]
    (loop [next-n (next-number n), l '(), s #{}]
      (if (contains? s next-n)
        (if (= (sort (util/digits (first l))) (sort d))
          (if (= (first l) n)
            l
            (conj l n))
          (conj (reverse l) n))
        (recur (next-number next-n) (conj l next-n) (conj s next-n))))))

(defn solve
  ([]
   (solve 1000000))
  ([upper]
   (->> (range 1 upper)
        (map #(sort (util/digits %)))
        (frequencies)
        (map #(vector (count (find-chain (first %))) (second %)))
        (filter #(= (first %) 60))
        (map #(second %))
        (apply +))))
