(ns project-euler.sol.p0073-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [project-euler.sol.p0073 :refer [solve]]))

(deftest test-p0073
  (testing "The upper is 8."
    (is (= 3 (solve 8))))
  (testing "The upper is 12000."
    (is (= 7295372 (solve 12000)))))
