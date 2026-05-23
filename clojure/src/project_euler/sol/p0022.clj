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
  (transduce (map #(- (int %) (dec (int \A)))) + word))

(defn solve
  ([]
   (solve "0022_names.txt"))
  ([fname]
   (->> (parse-data (util/read-data fname))
        (sort)
        (transduce (map-indexed #(* (inc %1) (worth %2))) +))))
