(ns project-euler.sol.p0062-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [project-euler.sol.p0062 :refer [solve]]))

(deftest test-p0062
  (testing "Three permutations."
    (is (= 41063625 (solve 3))))
  (testing "Five permutations."
    (is (= 127035954683 (solve 5)))))
