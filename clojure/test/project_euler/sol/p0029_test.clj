(ns project-euler.sol.p0029-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [project-euler.sol.p0029 :refer [solve]]))

(deftest test-p0029
  (testing "The upper is 5."
    (is (= 15 (solve 5))))
  (testing "The upper is 100."
    (is (= 9183 (solve 100)))))
