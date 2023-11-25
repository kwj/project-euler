(ns project-euler.sol.p0061
  (:require
   [project-euler.lib.math :as math]
   [project-euler.lib.util :as util]))

(defn- get-valid-numbers
  [ns]
  (->> (drop-while #(< % 1000) ns)
       (take-while #(< % 10000))
       (filter #(>= (mod % 100) 10))))

(defn- make-kv-map
  [ns]
  (loop [kv-map {}
         kvs (map #(vector (quot % 100) (mod % 100)) ns)]
    (if (seq kvs)
      (let [[k v] (first kvs)]
        (recur (assoc kv-map k (conj (get kv-map k '()) v)) (rest kvs)))
      kv-map)))

(def ^:private route-tbl
  {3 (make-kv-map (get-valid-numbers math/triangle-numbers))
   4 (make-kv-map (get-valid-numbers math/square-numbers))
   5 (make-kv-map (get-valid-numbers math/pentagonal-numbers))
   6 (make-kv-map (get-valid-numbers math/hexagonal-numbers))
   7 (make-kv-map (get-valid-numbers math/heptagonal-numbers))
   8 (make-kv-map (get-valid-numbers math/octagonal-numbers))})

(defn- distinct-numbers?
  [path]
  (let [nums (->> (partition 2 1 path)
                  (map (fn [[x y]] (+ (* 100 x) y))))]
    (= (count nums) (count (distinct nums)))))

(defn- find-cycles
  [start route]
  (let [result (transient [])]
    (letfn [(dfs [route path]
              (if (seq route)
                (doseq [next-num (get (get route-tbl (first route)) (last path))]
                  (dfs (rest route) (conj path next-num)))
                (when (and (= (first path) (last path))
                           (distinct-numbers? path))
                  (conj! result (rest path)))))]
      (doseq [[k vs] (get route-tbl start)
              next-num vs]
        (dfs route (vector k next-num))))
    (persistent! result)))

(defn solve
  ([]
   (solve 8))
  ([n]
   {:pre [(<= 4 n 8)]}
   (loop [routes (util/permutation (range (dec n) 2 -1))
          answer '()]
     (if (seq routes)
       (let [result (find-cycles n (first routes))]
         (if (seq result)
           (recur (rest routes) (into answer result))
           (recur (rest routes) answer)))
       (if (= (count answer) 1)
         ;; sum(100*x{1} + x{2}, 100*x{2} + x{3}, ..., 100*x{n} + x{1})
         ;;   = sum(x{1}, x{2}, ..., x{n}) * 101
         (* (apply + (first answer)) 101)
         (assert false (format "Many answers (3..=%d) %s" n answer)))))))
