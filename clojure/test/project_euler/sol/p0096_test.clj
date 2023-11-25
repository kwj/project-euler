(ns project-euler.sol.p0096-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [project-euler.sol.p0096 :refer [solve]]))

(deftest test-p0096
  (testing "Using 0096_sudoku.txt."
    (is (= 24702 (solve)))))
