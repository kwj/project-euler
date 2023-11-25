(ns project-euler.sol.p0051
  (:require
   [project-euler.lib.math.prime :as prime]
   [project-euler.lib.util :as util]))

(defn- get-family-primes
  [p-digits start-num mask-lst]
  (let [p-vec (vec p-digits)
        nums (for [d (range start-num 10)]
               (loop [v p-vec
                      lst mask-lst]
                 (if (seq lst)
                   (recur (assoc v (first lst) d) (rest lst))
                   (util/undigits v))))]
    (filter #(prime/prime? %) nums)))

(defn- prime-family?
  [p size]
  (let [p-digits (util/digits p)]
    (loop [ns (range 0 (inc (- 10 size)))]
      (if (seq ns)
        (let [n (first ns)
              groups (for [mask (->> (util/find-all #(= % n) p-digits)
                                     (util/powerset)
                                     (filter #(and (zero? (mod (count %) 3))
                                                   (not (zero? (count %)))
                                                   (not (zero? (nth % 0))))))]
                       (get-family-primes p-digits n mask))]
          (if (some #(>= (count %) size) groups)
            true
            (recur (rest ns))))
        false))))

(defn solve
  ([]
   (solve 8))
  ([size]
   {:pre [(pos? size)]}
   (->> prime/prime-numbers
        (drop-while #(< % 1000))
        (drop-while #(not (prime-family? % size)))
        (first))))
