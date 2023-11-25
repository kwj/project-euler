(ns project-euler.sol.p0086
  (:require [project-euler.lib.math :as math]))

(defn solve
  ([]
   (solve 1000000))
  ([thr]
   (loop [c 3
          acc 0]
     (if (> acc thr)
       (dec c)
       (recur (inc c)
              (long (+ acc
                       (loop [ab (* c 2)
                              acc 0]
                         (if (< ab 2)
                           acc
                           (cond
                             (not (math/square? (+ (* c c) (* ab ab)))) (recur (dec ab) acc)
                             (<= ab c) (recur (dec ab) (+ acc (quot ab 2)))
                             :else (recur (dec ab) (- (+ acc (quot ab 2)) (- ab 1 c)))))))))))))
