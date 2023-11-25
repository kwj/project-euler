(ns project-euler.sol.p0079
  (:require
   [clojure.string :as str]
   [project-euler.lib.util :as util]))

(defn- parse-data-aux
  [m [x y z]]
  (-> (assoc m x (conj (or (m x) #{}) y z))
      (assoc y (conj (or (m y) #{}) z))))

(defn- parse-data
  [data]
  (->> (map #(str/split % #"") data)
       (reduce (fn [m tpl] (parse-data-aux m tpl)) {})))

(defn- dfs
  [m perm v]
  (letfn [(visit [temp visited node]
            (cond
              (some #(= % node) temp) (assert false "cycle found")
              (some #(= % node) visited) visited
              :else (if-let [dst (m node)]
                      (into [node] (reduce (fn [perm v] (visit (into [node] temp) perm v)) visited dst))
                      [node])))]
    (visit [] perm v)))

(defn solve
  ([]
   (solve (util/read-data "0079_keylog.txt")))
  ([data]
   (let [graph (parse-data data)]
     (str/join "" (reduce (fn [acc v] (dfs graph acc v)) [] (keys graph))))))


