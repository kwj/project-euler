(ns project-euler.sol.p0002-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [project-euler.sol.p0002 :refer [solve]]))

(deftest test-p0002
  (testing "The sum of even fibonacci numbers under 100."
    (is (= 44 (solve 100))))
  (testing "The sum of even fibonacci numbers under 4_000_000."
    (is (= 4613732 (solve 4000000)))))
