(ns project-euler.sol.p0060-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [project-euler.sol.p0060 :refer [solve]]))

(deftest test-p0060
  (testing "The group size is 4."
    (is (= 792 (solve 4))))
  (testing "The group size is 5."
    (is (= 26033 (solve 5)))))
