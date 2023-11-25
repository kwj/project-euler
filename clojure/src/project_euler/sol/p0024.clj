(ns project-euler.sol.p0024
  (:require
   [project-euler.lib.math :as math]))

(defn solve
  ([]
   (solve 1000000))
  ([ith]
   (loop [nodes (vec (range 10))
          idx (dec ith)
          acc 0]
     (if (pos? (count nodes))
       (let [blk (math/factorial (dec (count nodes)))
             cnt (quot idx blk)]
         (recur (vec (concat (subvec nodes 0 cnt) (subvec nodes (inc cnt))))
                (mod idx blk)
                (long (+ (* acc 10) (get nodes cnt)))))
       (format "%010d" acc)))))

