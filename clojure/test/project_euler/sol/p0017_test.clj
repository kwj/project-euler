(ns project-euler.sol.p0017-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [project-euler.sol.p0017 :refer [solve]]))

(deftest test-p0017
  (testing "The upper is 5."
    (is (= 19 (solve 5))))
  (testing "The upper is 1000."
    (is (= 21124 (solve 1000)))))
