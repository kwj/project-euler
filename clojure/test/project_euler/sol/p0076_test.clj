(ns project-euler.sol.p0076-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [project-euler.sol.p0076 :refer [solve]]))

(deftest test-p0076
  (testing "The upper is 5."
    (is (= 6 (solve (range 1 5) 5))))
  (testing "The upper is 100."
    (is (= 190569291 (solve (range 1 100) 100)))))
