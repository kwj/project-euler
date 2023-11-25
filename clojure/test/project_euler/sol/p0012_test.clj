(ns project-euler.sol.p0012-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [project-euler.sol.p0012 :refer [solve]]))

(deftest test-p0012
  (testing "The threshold is 5."
    (is (= 28 (solve 5))))
  (testing "The threshold is 500."
    (is (= 76576500 (solve 500)))))
