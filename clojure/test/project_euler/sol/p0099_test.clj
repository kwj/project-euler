(ns project-euler.sol.p0099-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [project-euler.sol.p0099 :refer [solve]]))

(deftest test-p0099
  (testing "Using 0099_base_exp.txt."
    (is (= 709 (solve)))))
