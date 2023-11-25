(ns project-euler.sol.p0001-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [project-euler.sol.p0001 :refer [solve]]))

(deftest test-p0001
  (testing "The sum of all the multiples of 3 or 5 below 10."
    (is (= 23 (solve 10))))
  (testing "The sum of all the multiples of 3 or 5 below 1000."
    (is (= 233168 (solve 1000)))))
