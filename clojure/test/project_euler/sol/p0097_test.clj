(ns project-euler.sol.p0097-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [project-euler.sol.p0097 :refer [solve]]))

(deftest test-p0097
  (testing "Problem 97."
    (is (= "8739992577" (solve)))))
