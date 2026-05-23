(ns project-euler.sol.p0067
  (:require
   [clojure.string :as str]
   [project-euler.lib.util :as util]))

(defn- parse-data
  [data]
  (->> (map #(str/split % #" ") data)
       (map #(map (fn [s] (parse-long s)) %))))

(defn solve
  ([]
   (solve "0067_triangle.txt"))
  ([fname]
   (->> (parse-data (util/read-data fname))
        (reverse)
        (reduce #(map + (map max %1 (rest %1)) %2))
        (first))))
