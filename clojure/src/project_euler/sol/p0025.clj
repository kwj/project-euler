(ns project-euler.sol.p0025
  (:require [project-euler.lib.math :as math]))

(def ^:private fibonacci-numbers
  "A lazy sequence of fibonacci numbers."
  (lazy-cat [1 1] (map #(+' %2 %1) fibonacci-numbers (rest fibonacci-numbers))))

(defn solve
  ([]
   (solve 1000))
  ([n-digits]
   (let [upper (math/pow 10 (dec n-digits))]
     (-> (drop-while #(< (second %) upper)
                     (map-indexed (fn [idx n] [(inc idx) n]) fibonacci-numbers))
         (first)
         (get 0)))))

