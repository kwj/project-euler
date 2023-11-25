(ns project-euler.sol.p0058-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [project-euler.sol.p0058 :refer [solve]]))

(deftest test-p0058
  (testing "Problem 58."
    (is (= 26241 (solve)))))
