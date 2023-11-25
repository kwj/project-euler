(ns project-euler.sol.p0020-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [project-euler.sol.p0020 :refer [solve]]))

(deftest test-p0020
  (testing "10!."
    (is (= 27 (solve 10))))
  (testing "100!."
    (is (= 648 (solve 100)))))
