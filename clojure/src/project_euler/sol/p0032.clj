(ns project-euler.sol.p0032
  (:require
   [project-euler.lib.math :as math]
   [project-euler.lib.util :as util]))

(defn solve
  []
  (letfn [(aux [a b]
            (util/undigits (concat (util/digits (* a b))
                                   (util/digits b)
                                   (util/digits a))))]
    (->> (concat (for [a (range 1001 10000)
                       b (range 2 10)
                       :when (and (not= (mod a 10) 0)
                                  (< (* a b) 10000)
                                  (math/pandigital-nz? (aux a b)))]
                   (* a b))
                 (for [a (range 101 1000)
                       b (range 11 100)
                       :when (and (not= (mod a 10) 0)
                                  (not= (mod b 10) 0)
                                  (< (* a b) 10000)
                                  (math/pandigital-nz? (aux a b)))]
                   (* a b)))
         (sort)
         (dedupe)
         (apply +))))

