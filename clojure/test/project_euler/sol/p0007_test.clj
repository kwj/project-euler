(ns project-euler.sol.p0007-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [project-euler.sol.p0007 :refer [solve]]))

(deftest test-p0007
  (testing "The 6th prime number."
    (is (= 13 (solve 6))))
  (testing "The 10001th prime number."
    (is (= 104743 (solve 10001)))))
