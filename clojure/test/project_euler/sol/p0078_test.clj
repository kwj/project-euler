(ns project-euler.sol.p0078-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [project-euler.sol.p0078 :refer [solve]]))

(deftest test-p0078
  (testing "The threshold is 7."
    (is (= 5 (solve 7))))
  (testing "The threshold is 1000000."
    (is (= 55374 (solve 1000000)))))
