(ns project-euler.sol.p0032-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [project-euler.sol.p0032 :refer [solve]]))

(deftest test-p0032
  (testing "Problem 32."
    (is (= 45228 (solve)))))
