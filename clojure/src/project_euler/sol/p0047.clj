(ns project-euler.sol.p0047
  (:require [project-euler.lib.math.prime :as prime]))

(defn solve
  ([]
   (solve 4))
  ([cont-nums]
   (loop [cnt 0, x 1]
     (cond
       (not= cont-nums (count (prime/factorize x))) (recur 0 (inc x))
       (= cnt (dec cont-nums)) (- x (dec cont-nums))
       :else (recur (inc cnt) (inc x))))))
