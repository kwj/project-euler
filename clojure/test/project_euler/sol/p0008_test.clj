(ns project-euler.sol.p0008-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [project-euler.sol.p0008 :refer [solve]]))

(deftest test-p0008
  (testing "Five adjacent digits."
    (is (= 40824 (solve 5))))
  (testing "Thirteen adjacent digits."
    (is (= 23514624000 (solve 13)))))
