(ns project-euler.sol.p0037-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [project-euler.sol.p0037 :refer [solve]]))

(deftest test-p0037
  (testing "Problem 37."
    (is (= 748317 (solve)))))
