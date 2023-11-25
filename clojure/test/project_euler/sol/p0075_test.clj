(ns project-euler.sol.p0075-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [project-euler.sol.p0075 :refer [solve]]))

(deftest test-p0075
  (testing "The upper is 48."
    (is (= 6 (solve 48))))
  (testing "The upper is 1500000."
    (is (= 161667 (solve 1500000)))))
