(ns project-euler.sol.p0052
  (:require
   [project-euler.lib.math :as math]
   [project-euler.lib.util :as util]))

(defn- six-permuted-multiples?
  [n]
  (letfn [(make-key [x] (sort (util/digits x)))]
    (->> (range 1 7)
         (map #(make-key (* n %)))
         (apply =))))

(defn solve
  []
  (->> (iterate inc 6)
       (map #(range (math/pow 10 (dec %)) (inc (quot (math/pow 10 %) 6))))
       (flatten)
       (drop-while #(not (six-permuted-multiples? %)))
       (first)
       (long)))
