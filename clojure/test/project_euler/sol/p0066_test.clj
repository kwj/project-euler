(ns project-euler.sol.p0066-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [project-euler.sol.p0066 :refer [solve]]))

(deftest test-p0066
  (testing "The upper is 7."
    (is (= 5 (solve 7))))
  (testing "The upper is 1000."
    (is (= 661 (solve 1000)))))
