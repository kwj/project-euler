(ns project-euler.sol.p0026-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [project-euler.sol.p0026 :refer [solve]]))

(deftest test-p0026
  (testing "The upper is 10."
    (is (= 7 (solve 10))))
  (testing "The upper is 300."
    (is (= 289 (solve 300))))
  (testing "The upper is 1000."
    (is (= 983 (solve 1000)))))
