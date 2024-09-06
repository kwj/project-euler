(ns project-euler.sol.p0083
  (:require
   [clojure.string :as str]
   [project-euler.lib.util :as util]))

(defn- parse-data
  ^long/2 [data]
  (->> (map #(str/split % #",") data)
       (map #(map parse-long %))
       (map #(into-array Long/TYPE %))
       (into-array)))

(defn- make-neighbor-map
  [n-rows n-cols]
  (let [items (for [r (range n-rows)
                    c (range n-cols)]
                [r c [[(dec r) c] [(inc r) c] [r (dec c)] [r (inc c)]]])]
    (reduce (fn [m [r c nbrs]]
              (assoc-in m [r c] (filter #(and (<= 0 (first %) (dec n-rows)) (<= 0 (second %) (dec n-cols))) nbrs)))
            {}
            items)))

(defn- make-distance-tbl
  ^long/2 [n-rows n-cols]
  (into-array (reduce (fn [acc _] (conj acc (long-array (repeat n-cols Long/MAX_VALUE)))) '() (range n-rows))))

(defn solve
  ([]
   (solve (util/read-data "0083_matrix.txt")))
  ([data]
   (let [matrix (parse-data data)
         rows (alength matrix)
         cols (alength ^longs (aget matrix 0))
         nbrs-map (make-neighbor-map rows cols)
         dist-tbl (make-distance-tbl rows cols)
         pq (java.util.PriorityQueue. (reify java.util.Comparator
                                        (compare [_this x y]
                                          (cond (> (get x 0) (get y 0)) 1
                                                (< (get x 0) (get y 0)) -1
                                                :else 0))))]
     ;; The `aset*` and `aget` functions with multi-dimensional indexes are still very slow on Clojure 1.11.
     ;; I therefore use a workaround.
     ;; [Refer] https://clojure.atlassian.net/browse/CLJ-1289
     (aset-long (aget dist-tbl 0) 0 (aget ^longs (aget matrix 0) 0))
     (.add pq [(aget dist-tbl 0 0) [0 0]])
     (while (> (.size pq) 0)
       (let [[d [i j]] (.poll pq)]
         (doseq [[x y] (get-in nbrs-map [i j])]
           (let [new-d (+ d (aget ^longs (aget matrix x) y))]
             (when (< new-d (aget ^longs (aget dist-tbl x) y))
               (aset-long (aget dist-tbl x) y new-d)
               (.add pq [new-d [x y]]))))))
     (aget ^longs (aget dist-tbl (dec rows)) (dec cols)))))
