(ns project-euler.sol.p0063-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [project-euler.sol.p0063 :refer [solve]]))

(deftest test-p0063
  (testing "Problem 63."
    (is (= 49 (solve)))))
