(ns project-euler.sol.p0055-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [project-euler.sol.p0055 :refer [solve]]))

(deftest test-p0055
  (testing "The upper is 10000"
    (is (= 249 (solve 10000)))))
