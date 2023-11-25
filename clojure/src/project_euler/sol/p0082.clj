(ns project-euler.sol.p0082
  (:require
   [clojure.string :as str]
   [project-euler.lib.util :as util]))

(defn- parse-data
  [data]
  (->> (map #(str/split % #",") data)
       (map #(map parse-long %))
       (apply map list) ; transposition
       (map #(into-array Long/TYPE %))))

(defn solve
  ([]
   (solve (util/read-data "0082_matrix.txt")))
  ([data]
   (let [matrix (parse-data data)]
     (loop [work (longs (first matrix))
            xs (next matrix)]
       (if-let [arr (longs (first xs))]
         (do (aset work 0 (+ (aget work 0) (aget arr 0)))
             (doseq [i (range 1 (alength arr))]
               (aset work i (+ (aget arr i)
                               (min (aget work (dec i)) (aget work i)))))
             (doseq [i (reverse (range (dec (alength arr))))]
               (aset work i (min (aget work i)
                                 (+ (aget arr i) (aget work (inc i))))))
             (recur work (next xs)))
         (apply min work))))))
