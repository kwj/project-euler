(ns project-euler.sol.p0097
  (:require [project-euler.lib.math :as math]))

(defn solve
  []
  (let [modulo (math/pow 10 10)]
    (format "%010d" (biginteger (mod (inc (* 28433 (math/powermod 2 7830457 modulo))) modulo)))))
