(ns project-euler.sol.p0024
  (:require
   [project-euler.lib.math :as my-math]))

(defn solve
  ([]
   (solve 1000000))
  ([ith]
   (loop [nodes (vec (range 10))
          idx (dec ith)
          acc 0]
     (if-not (empty? nodes)
       (let [blk (my-math/factorial (dec (count nodes)))
             cnt (quot idx blk)
             n (nth nodes cnt)]
         (recur (vec (remove #(= % n) nodes))
                (mod idx blk)
                (+ (* acc 10) n)))
       (format "%010d" acc)))))
