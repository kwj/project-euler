(ns project-euler.sol.p0090-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [project-euler.sol.p0090 :refer [solve]]))

(deftest test-p0090
  (testing "Problem 90."
    (is (= 1217 (solve)))))
