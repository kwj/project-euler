(ns project-euler.sol.p0057
  (:require [project-euler.lib.util :as util]))

(def ^:private continued-fraction
  (letfn [(aux [n d]
            (lazy-seq (cons [n d]
                            (aux (+' (*' 2 d) n) (+' d n)))))]
    (aux 3 2)))

(defn solve
  ([]
   (solve 1000))
  ([cnt]
   (->> (take cnt continued-fraction)
        (filter (fn [[n d]] (> (count (util/digits n)) (count (util/digits d)))))
        (count))))
