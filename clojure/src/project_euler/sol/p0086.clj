(ns project-euler.sol.p0086
  (:require [project-euler.lib.math :as math]))

(defn- aux
  ^long [^long c]
  (loop [ab (* c 2)
         acc 0]
    (if (> ab 1)
      (let [tmp (+ (* c c) (* ab ab))
            tmp-isqrt (math/isqrt-long tmp)]
        (if (= (* tmp-isqrt tmp-isqrt) tmp)
          (cond
            (<= ab c) (recur (dec ab) (+ acc (quot ab 2)))
            :else (recur (dec ab) (+ acc (- (quot ab 2) (- ab 1 c)))))
          (recur (dec ab) acc)))
      acc)))

(defn solve
  ([]
   (solve 1000000))
  ([thr]
   (loop [c 3
          cnt 0]
     (if (> cnt thr)
       (dec c)
       (recur (inc c) (+ cnt (aux c)))))))
