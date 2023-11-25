(ns project-euler.sol.p0083-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [project-euler.sol.p0083 :refer [solve]]))

(deftest test-p0083
  (testing "Using 0083_matrix.txt."
    (is (= 425185 (solve)))))
