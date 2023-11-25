(ns project-euler.sol.p0046-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [project-euler.sol.p0046 :refer [solve]]))

(deftest test-p0046
  (testing "Problem 46."
    (is (= 5777 (solve)))))
