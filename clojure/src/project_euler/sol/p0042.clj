(ns project-euler.sol.p0042
  (:require
   [clojure.string :as str]
   [project-euler.lib.math :as math]
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
   (solve (util/read-data "0042_words.txt")))
  ([data]
   (->> (parse-data data)
        (filter #(math/triangular? (worth %)))
        (count))))

