(ns project-euler.sol.p0082-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [project-euler.sol.p0082 :refer [solve]]))

(deftest test-p0082
  (testing "Using 0082_matrix.txt."
    (is (= 260324 (solve)))))
