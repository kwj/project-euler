(ns project-euler.sol.p0084-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [project-euler.sol.p0084 :refer [solve]]))

(deftest test-p0084
  (testing "4-sided dice."
    (is (= "101524" (solve 4)))))
