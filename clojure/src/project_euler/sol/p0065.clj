(ns project-euler.sol.p0065
  (:require [project-euler.lib.util :as util]))

(defn- c
  [n]
  (if (zero? (mod n 3))
    (quot (*' 2 n) 3)
    1))

(def ^:private numerator-seq
  (letfn [(aux [x y idx]
            (let [next-x (+' (*' x (c idx)) y)]
              (cons next-x
                    (lazy-seq (aux next-x x (inc idx))))))]
    (concat '(2 3) (aux 3 2 3))))

(defn solve
  ([]
   (solve 100))
  ([stop]
   {:pre [(pos? stop)]}
   (->> (nth numerator-seq (dec stop))
        (util/digits)
        (apply +))))
