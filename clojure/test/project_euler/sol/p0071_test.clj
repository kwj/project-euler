(ns project-euler.sol.p0071-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [project-euler.sol.p0071 :refer [solve]]))

(deftest test-p0071
  (testing "The upper is 8."
    (is (= 2 (solve 8))))
  (testing "The upper is 1000000."
    (is (= 428570 (solve 1000000)))))
