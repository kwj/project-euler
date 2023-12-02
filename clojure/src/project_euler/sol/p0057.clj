(ns project-euler.sol.p0057
  (:require [project-euler.lib.util :as util]))

;;;;  use recurrence relation:
;;;;    sqrt(2) = 1 + sqrt(2) - 1
;;;;            = 1 + 1 / ( 1 / (sqrt(2) - 1) )
;;;;            = 1 + 1 / ( (sqrt(2) + 1) / (2 - 1) )
;;;;            = 1 + 1 / (1 + sqrt(2))
;;;;    -->
;;;;    a{1} = 1 + 1/2
;;;;    a{n} = 1 + 1/(1 + a{n-1})    [n>1]
;;;;
;;;;  assume that b{n}/c{n} = a{n}
;;;;    b{1}/c{1} = 1 + 1/2 = 3/2
;;;;    b{n}/c{n} = 1 + 1/(1 + b{n-1}/c{n-1})
;;;;              = 1 + 1/((c{n-1) + b{n-1})/c{n-1})
;;;;              = 1 + c{n-1}/(c{n-1) + b{n-1})
;;;;              = (c{n-1) + b{n-1} + c{n-1))/(c{n-1) + b{n-1})
;;;;              = (2 * c{n-1} + b{n-1}) / (c{n-1) + b{n-1})

(def ^:private continued-fraction
  (letfn [(aux [n d]
            (lazy-seq (cons [n d]
                            (aux (+' (*' 2 d) n) (+' d n)))))]
    (aux 3 2)))

(defn solve
  ([]
   (solve 1000))
  ([cnt]
   (->> (take cnt continued-fraction)
        (filter (fn [[n d]] (> (count (util/digits n)) (count (util/digits d)))))
        (count))))
