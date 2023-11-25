(ns project-euler.sol.p0040-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [project-euler.sol.p0040 :refer [solve]]))

(deftest test-p0040
  (testing "Problem 40."
    (is (= 210 (solve)))))
