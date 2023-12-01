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
    (<= 0x61 c 0x7A) 2 ; lowercase letters
    (<= 0x41 c 0x5A) 5 ; uppercase letters
    (<= 0x21 c 0x7E) 1 ; printable characters (exclude letters)
    (= c 0x20) 3 ; space
    :else 0))

(defn- eval-score
  [data key]
  [(->> (map-indexed (fn [idx ch] (eval-char (bit-xor ch (nth key (mod idx 3)))))
                     data)
        (apply +))
   key])

(defn solve
  ([]
   (solve (util/read-data "0059_cipher.txt")))
  ([data]
   (let [encrypted-data (parse-data data)
         key (->> (util/cartesian-product (range (int \a) (inc (int \z)))
                                          (range (int \a) (inc (int \z)))
                                          (range (int \a) (inc (int \z))))
                  (map #(eval-score encrypted-data (vec %)))
                  (apply max-key first)
                  (second))]
     (->> (map-indexed (fn [idx ch] (bit-xor ch (nth key (mod idx 3))))
                       encrypted-data)
          (apply +)))))
