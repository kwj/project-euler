(ns project-euler.sol.p0051-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [project-euler.sol.p0051 :refer [solve]]))

(deftest test-p0051
  (testing "Problem 51."
    (is (= 121313 (solve)))))
