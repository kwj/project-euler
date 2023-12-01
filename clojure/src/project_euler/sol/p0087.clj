(ns project-euler.sol.p0087
  (:require
   [clojure.math]
   [project-euler.lib.math :as math]
   [project-euler.lib.math.prime :as prime]))

(defn solve
  ([]
   (solve 50000000))
  ([limit]
   (let [nums (for [x (take-while #(<= % (math/isqrt-long (- limit (math/pow 2 3) (math/pow 2 4))))
                                  prime/prime-numbers)
                    y (take-while #(<= % (math/floor (clojure.math/pow (double limit) (/ 1.0 3.0))))
                                  prime/prime-numbers)
                    z (take-while #(<= % (math/floor (clojure.math/pow (double limit) (/ 1.0 4.0))))
                                  prime/prime-numbers)
                    :let [tmp (+ (math/pow x 2) (math/pow y 3) (math/pow z 4))]
                    :when (< tmp limit)]
                tmp)]
     (count (set nums)))))
