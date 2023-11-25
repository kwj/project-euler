(ns project-euler.sol.p0038-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [project-euler.sol.p0038 :refer [solve]]))

(deftest test-p0038
  (testing "Problem 38."
    (is (= 932718654 (solve)))))
