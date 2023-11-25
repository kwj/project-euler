(ns project-euler.sol.p0030-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [project-euler.sol.p0030 :refer [solve]]))

(deftest test-p0030
  (testing "Fourth powers."
    (is (= 19316 (solve 4))))
  (testing "Fifth powers."
    (is (= 443839 (solve 5)))))
