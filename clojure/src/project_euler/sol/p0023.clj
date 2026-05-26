(ns project-euler.sol.p0023
  (:require
   [project-euler.lib.math.prime :as prime]))

(defn- make-abundant-tbl
  [upper]
  (->> (prime/make-sigma-tbl 1 upper)
       (map-indexed #(if (< %1 (- %2 %1)) %1 nil))
       (vec)))

(defn solve
  ([]
   (solve 28123))
  ([upper]
   (let [tbl (make-abundant-tbl upper)]
     (loop [xs (range 1 (inc upper))
            ab-lst '()
            acc 0]
       (if (seq xs)
         (let [x (first xs)
               ab-cand (quot x 2)]
           (if (and (even? x) (nth tbl ab-cand))
             (let [next-ab-lst (conj ab-lst ab-cand)]
               (if (some #(nth tbl (- x %)) next-ab-lst)
                 (recur (next xs) next-ab-lst acc)
                 (recur (next xs) next-ab-lst (+ acc x))))
             (if (some #(nth tbl (- x %)) ab-lst)
               (recur (next xs) ab-lst acc)
               (recur (next xs) ab-lst (+ acc x)))))
         acc)))))
