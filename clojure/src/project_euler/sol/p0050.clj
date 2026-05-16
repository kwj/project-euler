(ns project-euler.sol.p0050
  (:require [project-euler.lib.math.prime :as prime]))

(def ^:private cs-primes (reductions (fn [acc n] (+ acc n)) 0 prime/prime-numbers))

(defn- get-max-offset
  [limit]
  ((comp dec count take-while) #(< % limit) cs-primes))

(defn solve
  ([]
   (solve 1000000))
  ([limit]
   {:pre [(> limit 2)]}
   (loop [left 0
          k (get-max-offset limit)]
     (let [diff (- (nth cs-primes (+ left k)) (nth cs-primes left))]
       (cond
         (>= diff limit) (recur 0 (dec k))
         (prime/prime? diff) diff
         :else (recur (inc left) k))))))
