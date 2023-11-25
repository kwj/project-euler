(ns project-euler.sol.p0028-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [project-euler.sol.p0028 :refer [solve]]))

(deftest test-p0028
  (testing "5x5 spiral."
    (is (= 101 (solve 5))))
  (testing "1001x1001 spiral."
    (is (= 669171001 (solve 1001)))))
