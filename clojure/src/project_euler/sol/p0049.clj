(ns project-euler.sol.p0049
  (:require
   [project-euler.lib.math :as math]
   [project-euler.lib.math.prime :as prime]
   [project-euler.lib.util :as util]))

(defn- get-prime-tbl
  [ndigits]
  (loop [primes (reverse (prime/primes (math/pow 10 (dec ndigits)) (math/pow 10 ndigits)))
         tbl {}]
    (if-let [p (first primes)]
      (let [k (util/undigits (sort (util/digits p)))]
        (recur (rest primes) (assoc tbl k (cons p (get tbl k '())))))
      tbl)))

(defn- valid-set?
  [[x y z]]
  (and (= (- y x) (- z y)) (not= x 1487) (not= z 8147)))

(defn solve
  ([]
   (let [ndigits 4
         xf (comp (filter #(>= (count (second %)) 3))
                  (mapcat #(util/combination 3 (second %)))
                  (filter valid-set?))
         ;; The problem statement mentions there exists exactly only one answer.
         [x y z] (first (sequence xf (seq (get-prime-tbl ndigits))))]
     (+ (* x (math/pow 10 (* ndigits 2))) (* y (math/pow 10 ndigits)) z))))
