(ns project-euler.sol.p0099
  (:require
   [clojure.math :as math]
   [clojure.string :as str]
   [project-euler.lib.util :as util]))

(defn- parse-data
  [data]
  (->> (map #(str/split % #",") data)
       (map #(map parse-long %))))

(defn solve
  ([]
   (solve "0099_base_exp.txt"))
  ([fname]
   (let [xf (comp (map (fn [[b e]] (* e (math/log10 b))))
                  (map-indexed vector))]
     (->> (apply max-key second (into [] xf (parse-data (util/read-data fname))))
          (first)
          (inc)))))
