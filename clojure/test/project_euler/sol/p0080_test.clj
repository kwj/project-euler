(ns project-euler.sol.p0080-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [project-euler.sol.p0080 :refer [solve]]))

(deftest test-p0080
  (testing "The upper is 2."
    (is (= 475 (solve 2 100))))
  (testing "The upper is 100."
    (is (= 40886 (solve 100 100)))))
