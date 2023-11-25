(ns project-euler.sol.p0098
  (:require
   [clojure.set]
   [clojure.string :as str]
   [project-euler.lib.math :as math]
   [project-euler.lib.util :as util]))

(def ^:private sq-tbl (atom {}))

(defn- get-squares
  [n-digits]
  (if-let [squares (get @sq-tbl n-digits)]
    squares
    (do (swap! sq-tbl assoc n-digits (->> math/square-numbers
                                          (drop-while #(< % (math/pow 10 (dec n-digits))))
                                          (take-while #(< % (math/pow 10 n-digits)))
                                          (map str)
                                          (set)))
        (recur n-digits))))

(defn- parse-data
  [data]
  (loop [words (-> (first data)
                   (str/replace #"\"" "")
                   (str/split #","))
         result (transient {})]
    (if-let [w (first words)]
      (let [k (str/join (sort w))]
        (recur (next words)
               (assoc! result k (conj (get result k '()) w))))
      (->> (filter #(> (count (second %)) 1) (persistent! result))
           (into {})))))

(defn- find-max-square-from-words
  [words]
  (let [squares (get-squares (count (first words)))]
    (letfn [(aux [[w1 w2]]
              (loop [sqs squares
                     result 0]
                (if-let [sq (first sqs)]
                  (let [trans (clojure.set/map-invert (zipmap sq w1))]
                    (if (not= sq (str/join (map #(get trans %) w1)))
                      (recur (next sqs) result)
                      (let [tmp (str/join (map #(get trans %) w2))]
                        (if (contains? squares tmp)
                          (recur (next sqs) (long (max result (parse-long sq) (parse-long tmp))))
                          (recur (next sqs) result)))))
                  result)))]
      (->> (util/combination 2 words)
           (map aux)
           (apply max)))))

(defn solve
  ([]
   (solve (util/read-data "0098_words.txt")))
  ([data]
   (->> (parse-data data)
        (vals)
        (map find-max-square-from-words)
        (apply max))))
