(ns project-euler.sol.p0052-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [project-euler.sol.p0052 :refer [solve]]))

(deftest test-p0052
  (testing "Problem 52."
    (is (= 142857 (solve)))))
