(ns project-euler.sol.p0059
  (:require
   [clojure.string :as str]
   [project-euler.lib.util :as util]))

(defn- parse-data
  [data]
  (->> (str/split (first data) #",")
       (map parse-long)))

(defn- eval-char
  [c]
  (cond
    (= c 0x20) 3 ; space
    (<= 0x41 c 0x5A) 5 ; uppercase letters
    (<= 0x61 c 0x7A) 2 ; lowercase letters
    (<= 0x21 c 0x7E) 1 ; printable characters (exclude letters)
    :else 0))

(defn- eval-score
  [cipher-data key]
  (let [plain-text (map bit-xor cipher-data (cycle key))
        score (->> (map #(eval-char %) plain-text)
                   (apply +))]
    [score (apply + plain-text) key]))

(defn solve
  ([]
   (solve (util/read-data "0059_cipher.txt")))
  ([data]
   (let [encrypted-data (parse-data data)]
     (->> (util/cartesian-product (range (int \a) (inc (int \z)))
                                  (range (int \a) (inc (int \z)))
                                  (range (int \a) (inc (int \z))))
          (map #(eval-score encrypted-data %))
          (apply max-key first)
          (second)))))
