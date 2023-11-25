(ns project-euler.sol.p0016-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [project-euler.sol.p0016 :refer [solve]]))

(deftest test-p0016
  (testing "2 raise to the power of 15."
    (is (= 26 (solve 15))))
  (testing "2 raise to the power of 1000."
    (is (= 1366 (solve 1000)))))
