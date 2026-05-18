(ns project-euler.sol.p0052
  (:require
   [project-euler.lib.math :as math]
   [project-euler.lib.util :as util]))

(defn- six-permuted-multiples?
  [n]
  (letfn [(make-key [x] (sort (util/digits x)))]
    (let [key (make-key n)]
      (every? #(= key (make-key (* n %))) (range 2 7)))))

(defn solve
  []
  (let [xf (comp (mapcat #(range (long (math/pow 10 (dec %))) (inc (quot (long (math/pow 10 %)) 6))))
                 (filter #(six-permuted-multiples? %))
                 (take 1))]
    (first (sequence xf (iterate inc 6)))))
