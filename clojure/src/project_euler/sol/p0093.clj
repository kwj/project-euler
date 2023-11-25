(ns project-euler.sol.p0093
  (:require [project-euler.lib.util :as util]))

(defn- make-numbers
  "Return an ascending list of all positive numbers computed from a given set of digits."
  [digits]
  (let [set-of-numbers (transient #{})]
    (letfn [(remove-at [idxs coll]
              (->> (remove (fn [[i _]] (some #(= i %) idxs)) (map-indexed vector coll))
                   (map #(nth % 1))))
            (aux [lst]
              (if (= (count lst) 1)
                (when (integer? (first lst))
                  (conj! set-of-numbers (first lst)))
                (doseq [[d1 d2 rest-lst] (for [[i d1] (map-indexed vector lst)
                                               [j d2] (map-indexed vector lst)
                                               :when (< i j)]
                                           [d1 d2 (remove-at [i j] lst)])]
                  (aux (cons (+ d1 d2) rest-lst))
                  (aux (cons (* d1 d2) rest-lst))
                  (aux (cons (- d1 d2) rest-lst))
                  (aux (cons (- d2 d1) rest-lst))
                  (when (not (zero? d1))
                    (aux (cons (/ d2 d1) rest-lst)))
                  (when (not (zero? d2))
                    (aux (cons (/ d1 d2) rest-lst))))))]
      (aux digits)
      (->> (persistent! set-of-numbers)
           (filter #(pos? %))
           (sort)))))

(defn- get-consec-length
  "Return the number of consecutive numbers from 1.
  If there is no consecutive numbers, return 0."
  [lst]
  (let [consec-seq (->> (make-numbers lst)
                        (map-indexed #(vector (inc %1) %2))
                        (take-while #(= (nth % 0) (nth % 1))))]
    (if (seq consec-seq)
      (first (last consec-seq))
      0)))

(defn solve
  []
  (->> (util/combination 4 (range 1 10))
       (map #(vector (get-consec-length %) %))
       (apply max-key first)
       (second)
       (reverse)
       (util/undigits)))
