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
  (transduce (map #(- (int %) (dec (int \A)))) + word))

(defn solve
  ([]
   (solve "0042_words.txt"))
  ([fname]
   (->> (parse-data (util/read-data fname))
        (filter #(math/triangular? (worth %)))
        (count))))
