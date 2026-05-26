(ns project-euler.sol.p0097
  (:require [project-euler.lib.math :as my-math]))

(defn solve
  []
  (let [modulo (my-math/pow 10 10)]
    (format "%010d" (mod (inc (* 28433 (my-math/powermod 2 7830457 modulo))) modulo))))
