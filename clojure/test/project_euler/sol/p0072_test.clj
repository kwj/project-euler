(ns project-euler.sol.p0072-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [project-euler.sol.p0072 :refer [solve]]))

(deftest test-p0072
  (testing "The upper is 8."
    (is (= 21 (solve 8))))
  (testing "The upper is 1000000."
    (is (= 303963552391 (solve 1000000)))))
