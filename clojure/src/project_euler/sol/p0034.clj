(ns project-euler.sol.p0034
  (:require
   [clojure.math]
   [project-euler.lib.math :as math]
   [project-euler.lib.util :as util]))

;;; the maximum n-digits number is n * 9!
;;;
;;;     10 ** (n-1) <= n * 9!
;;;  -> n - 1 <= log10(n * 9!)
;;;  -> n - 1 <= log10(n) + log10(9!)
;;;  -> n <= log10(n) + log10(9!) + 1
(defn- get-max-ndigits
  []
  (->> (iterate inc 3)
       (drop-while #(< % (+ (clojure.math/log10 %) (clojure.math/log10 (math/factorial 9)) 1)))
       (first)
       (dec)))

(defn solve
  []
  (let [fact-tbl (vec (map #(long (math/factorial %)) (range 10)))]
    (->> (range 2 (inc (get-max-ndigits)))
         (map #(util/combination-with-repetition % (range 10)))
         (apply concat)
         (map (fn [tpl] [tpl (apply + (map #(get fact-tbl %) tpl))]))
         (map (fn [[tpl n]] (if (= tpl (sort (util/digits n))) n 0)))
         (apply +))))

