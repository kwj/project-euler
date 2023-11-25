(ns project-euler.sol.p0091-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [project-euler.sol.p0091 :refer [solve]]))

(deftest test-p0091
  (testing "The area is 2x2."
    (is (= 14 (solve 2 2))))
  (testing "The upper is 50x50."
    (is (= 14234 (solve 50 50)))))
