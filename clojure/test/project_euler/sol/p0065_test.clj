(ns project-euler.sol.p0065-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [project-euler.sol.p0065 :refer [solve]]))

(deftest test-p0065
  (testing "The 10th convergent."
    (is (= 17 (solve 10))))
  (testing "The 100th convergent."
    (is (= 272 (solve 100)))))
