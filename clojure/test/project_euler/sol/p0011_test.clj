(ns project-euler.sol.p0011-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [project-euler.sol.p0011 :refer [solve]]))

(deftest test-p0011
  (testing "The length of adjacent numbers is 4."
    (is (= 70600674 (solve 4)))))
