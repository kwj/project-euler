(ns project-euler.sol.p0047
  (:require [project-euler.lib.math.prime :as prime]))

(defn solve
  ([]
   (solve 4))
  ([n-factors]
   (let [target (dec n-factors)]
     (loop [cnt 0
            x 1]
       (cond
         (not= n-factors (count (prime/factorize x))) (recur 0 (inc x))
         (= cnt target) (- x target)
         :else (recur (inc cnt) (inc x)))))))
