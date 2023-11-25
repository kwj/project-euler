(ns project-euler.sol.p0095-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [project-euler.sol.p0095 :refer [solve]]))

(deftest test-p0095
  (testing "Problem 95."
    (is (= 14316 (solve)))))
