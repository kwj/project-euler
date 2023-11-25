(ns project-euler.sol.p0031-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [project-euler.sol.p0031 :refer [solve]]))

(deftest test-p0031
  (testing "UK pound and pence."
    (is (= 73682 (solve [1 2 5 10 20 50 100 200] 200)))))
