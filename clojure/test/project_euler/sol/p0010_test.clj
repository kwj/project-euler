(ns project-euler.sol.p0010-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [project-euler.sol.p0010 :refer [solve]]))

(deftest test-p0010
  (testing "The upper is 10."
    (is (= 17 (solve 10))))
  (testing "The upper is 2000000."
    (is (= 142913828922 (solve 2000000)))))
