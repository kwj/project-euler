(ns project-euler.sol.p0018-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest testing is]]
   [project-euler.sol.p0018 :refer [solve]]))

(def ^:private example-triangle
  (->> ["03"
        "07 04"
        "02 04 06"
        "08 05 09 03"]
       (map #(str/split % #"\s"))
       (map #(map (fn [s] (parse-long s)) %))))

(deftest test-p0018
  (testing "Example triangle."
    (is (= 23 (solve example-triangle))))
  (testing "Problem triangle."
    (is (= 1074 (solve)))))
