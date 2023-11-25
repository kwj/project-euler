(ns project-euler.sol.p0036-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [project-euler.sol.p0036 :refer [solve]]))

(deftest test-p0036
  (testing "The upper is 1000000."
    (is (= 872187 (solve 1000000)))))
