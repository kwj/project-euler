(ns project-euler.sol.p0017)

(def ^:private nchars-1000 (count "thousand"))
(def ^:private nchars-100 (count "hundred"))
(def ^:private nchars-and (count "and"))

(def ^:private under-20
  (mapv count
        ["" "one" "two" "three" "four" "five" "six" "seven" "eight" "nine" "ten"
         "eleven" "twelve" "thirteen" "fourteen" "fifteen" "sixteen" "seventeen"
         "eighteen" "nineteen"]))

(def ^:private mults-10
  (mapv count
        ["" "" "twenty" "thirty" "forty" "fifty" "sixty" "seventy" "eighty" "ninety"]))

(defn- count-letters
  [n]
  {:pre [(pos? n) (<= n 1000)]}
  (cond
    (= n 1000) (+ (nth under-20 1) nchars-1000)
    (< n 20) (nth under-20 n)
    (< n 100) (let [q (quot n 10)
                    r (mod n 10)]
                (+ (nth mults-10 q) (nth under-20 r)))
    :else (let [q (quot n 100)
                r (mod n 100)]
            (+ (+ (nth under-20 q) nchars-100)
               (if (zero? r)
                 0
                 (+ nchars-and (count-letters r)))))))

(defn solve
  ([]
   (solve 1000))
  ([upper]
   (apply + (map count-letters (range 1 (inc upper))))))
