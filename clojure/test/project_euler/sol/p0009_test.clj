(ns project-euler.sol.p0009-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [project-euler.sol.p0009 :refer [solve]]))

(deftest test-p0009
  (testing "The perimeter is 12 (3 + 4 + 5)."
    (is (= 60 (solve 12))))
  (testing "The perimeter is 36 (9 + 12 + 15)."
    (is (= 1620 (solve 36))))
  (testing "The perimeter is 1000."
    (is (= 31875000 (solve 1000)))))
