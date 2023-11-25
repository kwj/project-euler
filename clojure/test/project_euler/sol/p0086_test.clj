(ns project-euler.sol.p0086-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [project-euler.sol.p0086 :refer [solve]]))

(deftest test-p0086
  (testing "The threshold is 1975."
    (is (= 100 (solve 1975))))
  (testing "The threshold is 1000000."
    (is (= 1818 (solve 1000000)))))
