(ns project-euler.sol.p0025-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [project-euler.sol.p0025 :refer [solve]]))

(deftest test-p0025
  (testing "3-digits."
    (is (= 12 (solve 3))))
  (testing "1000-digits."
    (is (= 4782 (solve 1000)))))
