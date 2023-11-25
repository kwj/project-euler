(ns project-euler.sol.p0035-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [project-euler.sol.p0035 :refer [solve]]))

(deftest test-p0035
  (testing "The upper is 100."
    (is (= 13 (solve 100))))
  (testing "The upper is 1000000"
    (is (= 55 (solve 1000000)))))
