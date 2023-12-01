;;; Note
;;;   This implementation cuts corners on processing for looping numbers.
;;;   It therefore doesn't work well for all chains.

(ns project-euler.sol.p0074
  (:require
   [project-euler.lib.math :as math]
   [project-euler.lib.util :as util]))

(defn n-perms [lst]
  (long (reduce (fn [acc x] (quot acc (math/factorial x)))
                (math/factorial (apply + lst))
                lst)))

(defn- num-of-valid-numbers
  [digit-lst]
  (let [n-digits (count digit-lst)
        digit-map (frequencies digit-lst)
        n-ones (get digit-map 1 0)]
    (letfn [(aux [n-zero n-one lst]
              (if (= n-digits n-zero)
                0
                (* (math/binomial (dec n-digits) n-zero)
                   (n-perms (conj lst n-one)))))]
      (apply + (map #(aux (- n-ones %) % (vals (dissoc digit-map 1)))
                    (range 0 (inc n-ones)))))))

(defn- next-number
  [n]
  (let [fact-tbl [1 1 2 6 24 120 720 5040 40320 362880]]
    (if (zero? n)
      (nth fact-tbl 0)
      (loop [n n
             acc 0]
        (if (zero? n)
          acc
          (recur (quot n 10) (long (+ acc (nth fact-tbl (mod n 10))))))))))

(defn- find-chain
  [digit-lst]
  (let [init-num (util/undigits digit-lst)]
    (loop [next-n (next-number init-num)
           l '()
           s #{}]
      (if (contains? s next-n)
        (if (= (sort (util/digits (first l))) digit-lst)
          (if (= (first l) init-num)
            l
            (conj l init-num))
          (conj (reverse l) init-num))
        (recur (next-number next-n) (conj l next-n) (conj s next-n))))))

(defn solve
  ([]
   (solve 6))
  ([n-digit]
   (->> (map #(util/combination-with-repetition % (range 1 10)) (range 1 (inc n-digit)))
        (apply concat)
        (filter #(= (count (find-chain %)) 60))
        (map num-of-valid-numbers)
        (apply +))))
