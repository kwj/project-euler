(ns project-euler.sol.p0077-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [project-euler.sol.p0077 :refer [solve]]))

(deftest test-p0077
  (testing "The threshold is 4."
    (is (= 10 (solve 4))))
  (testing "The threshold is 5000."
    (is (= 71 (solve 5000)))))
