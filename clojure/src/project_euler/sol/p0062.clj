(ns project-euler.sol.p0062
  (:require
   [project-euler.lib.math :as math]
   [project-euler.lib.util :as util]))

(defn- make-key
  [n]
  (util/undigits (sort (util/digits n))))

(defn solve
  ([]
   (solve 5))
  ([cnt]
   (loop [ns (iterate inc 1)
          tbl {}]
     (let [cube (long (math/pow (first ns) 3))
           key (make-key cube)
           data (get tbl key [])]
       (if (= (count data) (dec cnt))
         (first data)
         (recur (rest ns) (assoc tbl key (conj data cube))))))))
