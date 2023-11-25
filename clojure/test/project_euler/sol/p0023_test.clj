(ns project-euler.sol.p0023-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [project-euler.sol.p0023 :refer [solve]]))

(deftest test-p0023
  (testing "The upper is 28123."
    (is (= 4179871 (solve 28123)))))
