(ns project-euler.sol.p0099
  (:require
   [clojure.math]
   [clojure.string :as str]
   [project-euler.lib.util :as util]))

(defn- parse-data
  [data]
  (->> (map #(str/split % #",") data)
       (map #(map parse-long %))))

(defn solve
  ([]
   (solve (util/read-data "0099_base_exp.txt")))
  ([data]
   (let [xf (comp (map (fn [[b e]] (* e (clojure.math/log10 b))))
                  (map-indexed vector))]
     (->> (apply max-key second (eduction xf (parse-data data)))
          (first)
          (inc)))))
