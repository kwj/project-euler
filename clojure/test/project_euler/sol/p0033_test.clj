(ns project-euler.sol.p0033-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [project-euler.sol.p0033 :refer [solve]]))

(deftest test-p0033
  (testing "Problem 33."
    (is (= 100 (solve)))))
