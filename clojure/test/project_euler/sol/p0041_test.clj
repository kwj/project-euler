(ns project-euler.sol.p0041-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [project-euler.sol.p0041 :refer [solve]]))

(deftest test-p0041
  (testing "Problem 41."
    (is (= 7652413 (solve)))))
