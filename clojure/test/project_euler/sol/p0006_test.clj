(ns project-euler.sol.p0006-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [project-euler.sol.p0006 :refer [solve]]))

(deftest test-p0006
  (testing "The upper is 10."
    (is (= 2640 (solve 10))))
  (testing "The upper is 100."
    (is (= 25164150 (solve 100)))))
