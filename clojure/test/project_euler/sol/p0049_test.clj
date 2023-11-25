(ns project-euler.sol.p0049-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [project-euler.sol.p0049 :refer [solve]]))

(deftest test-p0049
  (testing "Problem 49."
    (is (= 296962999629 (solve)))))
