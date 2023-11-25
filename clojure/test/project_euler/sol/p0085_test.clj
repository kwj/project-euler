(ns project-euler.sol.p0085-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [project-euler.sol.p0085 :refer [solve]]))

(deftest test-p0085
  (testing "The target is 18."
    (is (= 6 (solve 18))))
  (testing "The target is 2000000."
    (is (= 2772 (solve 2000000)))))
