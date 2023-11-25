(ns project-euler.sol.p0057-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [project-euler.sol.p0057 :refer [solve]]))

(deftest test-p0057
  (testing "The limit is 8."
    (is (= 1 (solve 8))))
  (testing "The upper is 1000."
    (is (= 153 (solve 1000)))))
