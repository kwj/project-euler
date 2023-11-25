(ns project-euler.sol.p0022
  (:require
   [clojure.string :as str]
   [project-euler.lib.util :as util]))

(defn- parse-data
  [data]
  (-> (first data)
      (str/replace #"\"" "")
      (str/split #",")))

(defn- worth
  [word]
  (->> (map #(- (int %) (dec (int \A))) word)
       (apply +)))

(defn solve
  ([]
   (solve (util/read-data "0022_names.txt")))
  ([data]
   (->> (parse-data data)
        (sort)
        (map-indexed #(* (inc %1) (worth %2)))
        (apply +))))

