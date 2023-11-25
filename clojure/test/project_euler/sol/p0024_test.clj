(ns project-euler.sol.p0024-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [project-euler.sol.p0024 :refer [solve]]))

(deftest test-p0024
  (testing "The 1st digits."
    (is (= "0123456789" (solve 1))))
  (testing "The 1000000th digits."
    (is (= "2783915460" (solve 1000000)))))
