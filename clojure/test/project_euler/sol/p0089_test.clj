(ns project-euler.sol.p0089-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [project-euler.sol.p0089 :refer [solve]]))

(deftest test-p0089
  (testing "Using 0089_roman.txt."
    (is (= 743 (solve)))))
