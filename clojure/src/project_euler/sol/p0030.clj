(ns project-euler.sol.p0030
  (:require
   [project-euler.lib.math :as math]
   [project-euler.lib.util :as util]))

(defn- get-max-ndigits
  [exp]
  (let [k (math/pow 9 exp)]
    (->> (iterate inc 2)
         (drop-while #(> (* k %) (math/pow 10 (dec %))))
         (first)
         (dec))))

(defn solve
  ([]
   (solve 5))
  ([exp]
   {:pre [(> exp 1)]}
   (let [pow-tbl (vec (map #(long (math/pow % exp)) (range 10)))]
     (->> (range 2 (inc (get-max-ndigits exp)))
          (map #(util/combination-with-repetition % (range 10)))
          (apply concat)
          (map (fn [tpl] [tpl (apply + (map #(get pow-tbl %) tpl))]))
          (map (fn [[tpl n]] (if (= tpl (sort (util/digits n))) n 0)))
          (apply +)))))

