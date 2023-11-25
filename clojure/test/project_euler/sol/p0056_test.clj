(ns project-euler.sol.p0056-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [project-euler.sol.p0056 :refer [solve]]))

(deftest test-p0056
  (testing "Problem 56."
    (is (= 972 (solve 100)))))
