(ns project-euler.sol.p0050-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [project-euler.sol.p0050 :refer [solve]]))

(deftest test-p0050
  (testing "The upper is 100."
    (is (= 41 (solve 100))))
  (testing "The upper is 500."
    (is (= 499 (solve 500))))
  (testing "The upper is 1000."
    (is (= 953 (solve 1000))))
  (testing "The upper is 500."
    (is (= 9521 (solve 10000))))
  (testing "The upper is 500."
    (is (= 997651 (solve 1000000)))))
