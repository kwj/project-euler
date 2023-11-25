(ns project-euler.sol.p0100-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [project-euler.sol.p0100 :refer [solve]]))

(deftest test-p0100
  (testing "The threshold is 1000000000000."
    (is (= 756872327473 (solve 1000000000000)))))
