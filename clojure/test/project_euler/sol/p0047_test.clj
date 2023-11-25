(ns project-euler.sol.p0047-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [project-euler.sol.p0047 :refer [solve]]))

(deftest test-p0047
  (testing "Two consecutive numbers."
    (is (= 14 (solve 2))))
  (testing "Three consecutive numbers."
    (is (= 644 (solve 3))))
  (testing "Four consecutive numbers."
    (is (= 134043 (solve 4)))))
