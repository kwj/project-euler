(ns project-euler.sol.p0003-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [project-euler.sol.p0003 :refer [solve]]))

(deftest test-p0003
  (testing "The number is 13195."
    (is (= 29 (solve 13195))))
  (testing "The number is 600851475143."
    (is (= 6857 (solve 600851475143)))))
