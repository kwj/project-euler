(ns project-euler.sol.p0053-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [project-euler.sol.p0053 :refer [solve]]))

(deftest test-p0053
  (testing "The upper is 100."
    (is (= 4 (solve 23 1000000))))
  (testing "The upper is 500."
    (is (= 4075 (solve 100 1000000)))))
