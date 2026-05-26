(ns project-euler.sol.p0034
  (:require
   [clojure.math :as math]
   [project-euler.lib.math :as my-math]
   [project-euler.lib.util :as util]))

;;; the maximum n-digits number is n * 9!
;;;
;;;     10 ** (n-1) <= n * 9!
;;;  -> n - 1 <= log10(n * 9!)
;;;  -> n - 1 <= log10(n) + log10(9!)
;;;  -> n <= log10(n) + log10(9!) + 1
(def ^:private max-ndigits
  (->> (iterate inc 3)
       (drop-while #(< % (+ (math/log10 %) (math/log10 (my-math/factorial 9)) 1)))
       (first)
       (dec)))

(defn solve
  []
  (let [fact-tbl (mapv my-math/factorial (range 10))
        xf (comp (map #(util/combination-with-repetition % (range 10)))
                 cat
                 (map (fn [tpl] [tpl (apply + (map #(get fact-tbl %) tpl))]))
                 (map (fn [[tpl n]] (if (= tpl (sort (util/digits n))) n 0))))]
    (transduce xf + (range 2 (inc max-ndigits)))))
