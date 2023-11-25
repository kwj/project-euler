(ns project-euler.sol.p0081
  (:require
   [clojure.string :as str]
   [project-euler.lib.util :as util]))

(defn- parse-data
  [data]
  (->> (map #(str/split % #",") data)
       (map #(map parse-long %))
       (map-indexed (fn [idx l]
                      (if (zero? idx)
                        (reductions (fn [acc n] (+ acc n)) l)
                        l)))
       (map #(cons Long/MAX_VALUE %))
       (map #(into-array Long/TYPE %))))

(defn solve
  ([]
   (solve (util/read-data "0081_matrix.txt")))
  ([data]
   (let [matrix (parse-data data)]
     (loop [work (longs (first matrix))
            xs (next matrix)]
       (if-let [arr (longs (first xs))]
         (do
           (doseq [i (range 1 (alength arr))]
             (if (< (aget arr (dec i)) (aget work i))
               (aset arr i (+ (aget arr i) (aget arr (dec i))))
               (aset arr i (+ (aget arr i) (aget work i)))))
           (recur arr (next xs)))
         (aget work (dec (alength work))))))))


