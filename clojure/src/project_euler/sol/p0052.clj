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
  (->> (iterate inc 6)
       (map #(range (long (math/pow 10 (dec %))) (inc (quot (long (math/pow 10 %)) 6))))
       (flatten)
       (filter #(six-permuted-multiples? %))
       (first)))
