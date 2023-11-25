(ns project-euler.sol.p0050
  (:require [project-euler.lib.math.prime :as prime]))

(def ^:private cs-primes (reductions (fn [acc n] (+ acc n)) 0 prime/prime-numbers))

(defn- get-init-cs-length
  [limit]
  (->> (map-indexed #(vector (inc %1) %2) cs-primes)
       (drop-while #(< (second %) limit))
       (ffirst)))

(defn- find-consec-primes
  [begin-value limit v]
  (drop-while #(let [x (- % begin-value)]
                 (or (>= x limit) (not (prime/prime? x))))
              v))

(defn solve
  ([]
   (solve 1000000))
  ([upper]
   {:pre [(> upper 2)]}
   (let [init-length (get-init-cs-length upper)
         begin-pos (dec init-length)]
     (loop [cnt init-length
            width 0
            ans 0]
       (let [cs-vec (vec (reverse (take cnt cs-primes)))]
         (if (>= (- (nth cs-vec (- begin-pos width)) (nth cs-vec begin-pos)) upper)
           ans
           (let [lst (find-consec-primes (nth cs-vec begin-pos)
                                         upper
                                         (subvec cs-vec 0 (- begin-pos width)))]
             (if (zero? (count lst))
               (recur (inc cnt) width ans)
               (recur (inc cnt)
                      (+ width (count lst))
                      (long (- (nth lst 0) (nth cs-vec begin-pos))))))))))))
