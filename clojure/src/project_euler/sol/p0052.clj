(ns project-euler.sol.p0052
  (:require
   [project-euler.lib.math :as my-math]
   [project-euler.lib.util :as util]))

(defn- six-permuted-multiples?
  [n]
  (letfn [(make-key [x] (sort (util/digits x)))]
    (let [key (make-key n)]
      (every? #(= key (make-key (* n %))) (range 2 7)))))

(defn solve
  []
  (let [xf (comp (mapcat #(range (my-math/pow 10 (dec %)) (inc (quot (my-math/pow 10 %) 6))))
                 (filter six-permuted-multiples?)
                 ;; The input source is an infinite sequence and the problem statement says
                 ;; to find the smallest positive integer. So, the `take` function below is
                 ;; used to terminate immediately once an answer is found.
                 (take 1))]
    (first (sequence xf (iterate inc 6)))))
