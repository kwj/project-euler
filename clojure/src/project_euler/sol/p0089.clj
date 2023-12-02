(ns project-euler.sol.p0089
  (:require
   [clojure.string :as str]
   [project-euler.lib.util :as util]))

;;;   step 1:
;;;     IIIIIIIII     IX
;;;     XXXXXXXXX     XC
;;;     CCCCCCCCC     CM
;;;
;;;   step 2:
;;;     VIIII         IX
;;;     LXXXX         XC
;;;     DCCCC         CM
;;;
;;;   step 3:
;;;     IIIII         V
;;;     XXXXX         L
;;;     CCCCC         D
;;;
;;;   step 4:
;;;     IIII          IV
;;;     XXXX          XL
;;;     CCCC          CD
(defn- replace-roman-nums
  "Replace from long Roman numerals."
  [s]
  (-> s
      (str/replace #"IIIIIIIII|XXXXXXXXX|CCCCCCCCC" "##") ; IX/XC/CM
      (str/replace #"VIIII|LXXXX|DCCCC" "##")             ; IX/XC/CM
      (str/replace #"IIIII|XXXXX|CCCCC" "#")              ; V/L/D
      (str/replace #"IIII|XXXX|CCCC" "##"))) ; IV/XL/CD

(defn solve
  ([]
   (solve (util/read-data "0089_roman.txt")))
  ([data]
   (->> (map #(- (count %) (count (replace-roman-nums %))) data)
        (apply +))))
