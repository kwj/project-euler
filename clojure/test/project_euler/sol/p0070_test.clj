(ns project-euler.sol.p0070-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [project-euler.sol.p0070 :refer [solve]]))

(deftest test-p0070
  (testing "Problem 70."
    (is (= 8319823 (solve)))))
