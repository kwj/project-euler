(ns project-euler.sol.p0021-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [project-euler.sol.p0021 :refer [solve]]))

(deftest test-p0021
  (testing "Under 10000."
    (is (= 31626 (solve 10000)))))
