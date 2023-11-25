(ns project-euler.sol.p0015-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [project-euler.sol.p0015 :refer [solve]]))

(deftest test-p0015
  (testing "The grid is 2x2."
    (is (= 6 (solve 2 2))))
  (testing "The grid is 20x20."
    (is (= 137846528820 (solve 20 20)))))
