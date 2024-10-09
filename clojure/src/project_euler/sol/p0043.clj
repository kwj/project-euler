(ns project-euler.sol.p0043
  (:require [clojure.string :as str]))

(defn solve
  []
  (->> (loop [ds [1 1 17 13 11 7 5 3 2 1]
              lst '("")]
         (if (seq ds)
           (let [d (first ds)]
             (recur (rest ds)
                    (for [x ["0" "1" "2" "3" "4" "5" "6" "7" "8" "9"]
                          s lst
                          :let [tmp-s (str x s)]
                          :when (and (not (str/includes? s x))
                                     (or (< (count tmp-s) 3)
                                         (zero? (mod (parse-long (subs tmp-s 0 3)) d))))]
                      tmp-s)))
           lst))
       (filter #(not (= "0" (subs % 0 1))))
       (map parse-long)
       (apply +)))
