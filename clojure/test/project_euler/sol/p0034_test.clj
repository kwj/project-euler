(ns project-euler.sol.p0034-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [project-euler.sol.p0034 :refer [solve]]))

(deftest test-p0034
  (testing "Problem 34."
    (is (= 40730 (solve)))))
