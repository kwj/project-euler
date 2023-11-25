(ns project-euler.sol.p0023
  (:require
   [project-euler.lib.math.prime :as prime]))

(defn- make-abundant-tbl
  [upper]
  (->> (prime/make-sigma-tbl 1 upper)
       (map-indexed #(< %1 (- %2 %1)))
       (vec)))

(defn solve
  ([]
   (solve 28123))
  ([upper]
   (let [tbl (make-abundant-tbl upper)
         abundant-set (atom #{})]
     (loop [i 1, acc 0]
       (if (<= i upper)
         (do (when (and (even? i) (get tbl (quot i 2)))
               (swap! abundant-set conj (quot i 2)))
             (if (some #(get tbl (- i %)) @abundant-set)
               (recur (inc i) acc)
               (recur (inc i) (+ acc i))))
         acc)))))

