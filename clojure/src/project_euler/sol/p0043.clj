(ns project-euler.sol.p0043
  (:require [clojure.string :as str]))

(defn solve
  []
  (let [divisors [1 1 17 13 11 7 5 3 2 1]
        xf (comp (filter #(not (= "0" (subs % 0 1))))
                 (map parse-long))]
    (letfn [(find-numbers [ds str-numbers]
              (if-let [d (first ds)]
                (recur (next ds)
                       (for [x ["0" "1" "2" "3" "4" "5" "6" "7" "8" "9"]
                             s str-numbers
                             :when (not (str/includes? s x))
                             :let [next-s (str x s)]
                             :when (or (< (count next-s) 3)
                                       (zero? (mod (parse-long (subs next-s 0 3)) d)))]
                         next-s))
                str-numbers))]
      (transduce xf + (find-numbers divisors '(""))))))
