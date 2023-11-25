(ns project-euler.sol.p0092-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [project-euler.sol.p0092 :refer [solve]]))

(deftest test-p0092
  (testing "The upper is 10."
    (is (= 7 (solve 10))))
  (testing "The upper is 10000000."
    (is (= 8581146 (solve 10000000)))))
