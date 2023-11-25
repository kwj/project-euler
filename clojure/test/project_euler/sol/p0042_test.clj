(ns project-euler.sol.p0042-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [project-euler.sol.p0042 :refer [solve]]))

(deftest test-p0042
  (testing "Using 0042_words.txt."
    (is (= 162 (solve)))))
