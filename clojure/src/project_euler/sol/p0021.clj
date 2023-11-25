(ns project-euler.sol.p0021
  (:require
   [project-euler.lib.math.prime :as prime]))

(defn- make-aliquot-sum-tbl
  [upper]
  (->> (prime/make-sigma-tbl 1 (dec upper))
       (map-indexed #(- %2 %1))
       (vec)))

(defn solve
  ([]
   (solve 10000))
  ([upper]
   (let [tbl (make-aliquot-sum-tbl upper)
         amicable? (fn [n] (and (> n (nth tbl n)) (= n (nth tbl (nth tbl n)))))]
     (->> (range 2 upper)
          (map #(if (amicable? %) (+ % (nth tbl %)) 0))
          (apply +)))))

