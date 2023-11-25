(ns project-euler.sol.p0005-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [project-euler.sol.p0005 :refer [solve]]))

(deftest test-p0005
  (testing "The upper is 10."
    (is (= 2520 (solve 10))))
  (testing "The upper is 20."
    (is (= 232792560 (solve 20)))))
