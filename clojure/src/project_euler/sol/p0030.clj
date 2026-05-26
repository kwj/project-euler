(ns project-euler.sol.p0030
  (:require
   [project-euler.lib.math :as my-math]
   [project-euler.lib.util :as util]))

(defn- get-max-ndigits
  [exp]
  (let [k (my-math/pow 9 exp)]
    (->> (iterate inc 2)
         (drop-while #(> (* k %) (my-math/pow 10 (dec %))))
         (first)
         (dec))))

(defn solve
  ([]
   (solve 5))
  ([exp]
   {:pre [(> exp 1)]}
   (let [pow-tbl (mapv #(my-math/pow % exp) (range 10))
         xf (comp (map #(util/combination-with-repetition % (range 10)))
                  cat
                  (map (fn [tpl] [tpl (apply + (map #(nth pow-tbl %) tpl))]))
                  (map (fn [[tpl n]] (if (= tpl (sort (util/digits n))) n 0))))]
     (transduce xf + (range 2 (inc (get-max-ndigits exp)))))))
