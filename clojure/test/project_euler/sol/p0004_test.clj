(ns project-euler.sol.p0004-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [project-euler.sol.p0004 :refer [solve]]))

(deftest test-p0004
  (testing "A product of two 2-digit numbers."
    (is (= 9009 (solve 2))))
  (testing "A product of two 3-digit numbers."
    (is (= 906609 (solve 3)))))
