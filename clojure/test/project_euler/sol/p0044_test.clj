(ns project-euler.sol.p0044-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [project-euler.sol.p0044 :refer [solve]]))

(deftest test-p0044
  (testing "Problem 44."
    (is (= 5482660 (solve)))))
