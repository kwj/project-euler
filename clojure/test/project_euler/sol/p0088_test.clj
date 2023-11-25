(ns project-euler.sol.p0088-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [project-euler.sol.p0088 :refer [solve]]))

(deftest test-p0088
  (testing "The upper is 6."
    (is (= 30 (solve 6))))
  (testing "The upper is 12."
    (is (= 61 (solve 12))))
  (testing "The upper is 12000."
    (is (= 7587457 (solve 12000)))))
