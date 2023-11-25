(ns project-euler.sol.p0074-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [project-euler.sol.p0074 :refer [solve]]))

(deftest test-p0074
  (testing "Problem 74."
    (is (= 402 (solve)))))
