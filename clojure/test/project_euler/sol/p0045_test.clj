(ns project-euler.sol.p0045-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [project-euler.sol.p0045 :refer [solve]]))

(deftest test-p0045
  (testing "1st number."
    (is (= 1 (solve 1))))
  (testing "2nd number."
    (is (= 40755 (solve 2))))
  (testing "3rd number."
    (is (= 1533776805 (solve 3)))))
