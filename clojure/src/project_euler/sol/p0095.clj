(ns project-euler.sol.p0095
  (:require [project-euler.lib.math.prime :as prime]))

(defn- make-aliquot-sum-tbl
  [upper]
  (->> (prime/make-sigma-tbl 1 (inc upper))
       (map-indexed #(- %2 %1))
       (vec)))

(defn solve
  ([]
   (solve 1000000))
  ([limit]
   (let [as-tbl (make-aliquot-sum-tbl limit)
         chache-tbl (long-array (inc limit) 0)
         chain-map (transient {})]
     (letfn [(update-chache-tbl [v s]
               (doseq [i s]
                 (aset-long chache-tbl i v))
               (when (pos? v)
                 (assoc! chain-map v (apply conj (get chain-map v []) s))))
             (check-number [n]
               (when (zero? (aget chache-tbl n))
                 (loop [n n
                        s []]
                   (cond
                     (or (= n 1) (> n limit) (not= (aget chache-tbl n) 0)) (update-chache-tbl -1 s)
                     (some #(= % n) s) (do (update-chache-tbl -1 (take-while #(not= % n) s))
                                           (let [lst (drop-while #(not= % n) s)]
                                             (update-chache-tbl (count lst) lst)))
                     :else (recur (get as-tbl n) (conj s n))))))]
       (doseq [n (range 2 (inc limit))]
         (check-number n)))
     (->> (apply max-key key (persistent! chain-map))
          (second)
          (apply min)))))
