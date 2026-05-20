(ns project-euler.sol.p0098
  (:require
   [clojure.set]
   [clojure.string :as str]
   [project-euler.lib.math :as math]
   [project-euler.lib.util :as util]))

(def ^:private sq-tbl (atom {}))

(defn- get-squares
  [n-digits]
  (if-let [result (get @sq-tbl n-digits)]
    result
    (let [xf (comp (drop-while #(< % (math/pow 10 (dec n-digits))))
                   (take-while #(< % (math/pow 10 n-digits)))
                   (map str))
          squares (set (eduction xf math/square-numbers))]
      (swap! sq-tbl assoc n-digits squares)
      squares)))

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
      (filter #(> (count %) 1) (vals (persistent! result))))))

(defn- find-max-anagram-square
  "Find the maximum square number from square anagram word pairs.
   If there is no square anagram pair in anagrams, return 0."
  [anagrams]
  (let [string-sq-numbers (get-squares (count (first anagrams)))]
    (letfn [(aux [[w1 w2]]
              (loop [sqs string-sq-numbers
                     result 0]
                (if-let [sq (first sqs)]
                  (let [trans (clojure.set/map-invert (zipmap sq w1))]
                    (if (not= sq (str/join (map #(get trans %) w1)))
                      (recur (next sqs) result)
                      (let [tmp (str/join (map #(get trans %) w2))]
                        (if (contains? string-sq-numbers tmp)
                          (recur (next sqs) (max result (parse-long sq) (parse-long tmp)))
                          (recur (next sqs) result)))))
                  result)))]
      (->> (util/combination 2 anagrams)
           (map aux)))))

(defn solve
  ([]
   (solve (util/read-data "0098_words.txt")))
  ([data]
   (->> (parse-data data)
        (mapcat find-max-anagram-square)
        (apply max))))
