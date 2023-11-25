(ns project-euler.sol.p0014-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [project-euler.sol.p0014 :refer [solve]]))

(deftest test-p0014
  (testing "The upper is 1000000."
    (is (= 837799 (solve 1000000)))))
