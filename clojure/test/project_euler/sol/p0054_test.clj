(ns project-euler.sol.p0054-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [project-euler.sol.p0054 :refer [solve]]))

(deftest test-p0054
  (testing "Using 0054_poker.txt."
    (is (= 376 (solve)))))
