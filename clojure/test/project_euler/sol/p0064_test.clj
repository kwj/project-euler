(ns project-euler.sol.p0064-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [project-euler.sol.p0064 :refer [solve]]))

(deftest test-p0064
  (testing "The upper is 13."
    (is (= 4 (solve 13))))
  (testing "The upper is 10000."
    (is (= 1322 (solve 10000)))))
