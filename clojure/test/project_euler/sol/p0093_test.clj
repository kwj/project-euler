(ns project-euler.sol.p0093-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [project-euler.sol.p0093 :refer [solve]]))

(deftest test-p0093
  (testing "Problem 93."
    (is (= 1258 (solve)))))
