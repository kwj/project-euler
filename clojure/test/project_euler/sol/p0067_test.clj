(ns project-euler.sol.p0067-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [project-euler.sol.p0067 :refer [solve]]))

(deftest test-p0067
  (testing "Using 0067_triangle.txt."
    (is (= 7273 (solve)))))
