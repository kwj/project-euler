(ns project-euler.sol.p0081-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [project-euler.sol.p0081 :refer [solve]]))

(deftest test-p0081
  (testing "Using 0081_matrix.txt."
    (is (= 427337 (solve)))))
