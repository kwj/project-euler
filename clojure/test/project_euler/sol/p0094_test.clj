(ns project-euler.sol.p0094-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [project-euler.sol.p0094 :refer [solve]]))

(deftest test-p0094
  (testing "The threshold is 16."
    (is (= 16 (solve 16))))
  (testing "The threshold is 1000000000."
    (is (= 518408346 (solve 1000000000)))))
