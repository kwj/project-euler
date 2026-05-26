(ns project-euler.sol.p0059
  (:require
   [clojure.string :as str]
   [project-euler.lib.util :as util]))

(defn- parse-data
  [data]
  (->> (str/split (first data) #",")
       (map parse-long)))

(defn- score-char
  [c]
  (cond
    (<= 0x61 c 0x7A) 2 ; lowercase letters
    (<= 0x41 c 0x5A) 5 ; uppercase letters
    (<= 0x21 c 0x7E) 1 ; printable characters (exclude letters)
    (= c 0x20) 3 ; space
    :else 0))

(defn- unencrypt-and-sum
  [f data key]
  (let [key-v (vec key)]
    (->> (map-indexed (fn [idx ch] (f (bit-xor ch (nth key-v (mod idx 3))))) data)
         (reduce +))))

(defn solve
  ([]
   (solve "0059_cipher.txt"))
  ([fname]
   (let [encrypted-data (parse-data (util/read-data fname))]
     (->> (util/cartesian-product (range (int \a) (inc (int \z)))
                                  (range (int \a) (inc (int \z)))
                                  (range (int \a) (inc (int \z))))
          (apply max-key #(unencrypt-and-sum score-char encrypted-data %))
          (unencrypt-and-sum identity encrypted-data)))))
