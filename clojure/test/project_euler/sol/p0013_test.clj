(ns project-euler.sol.p0013-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [project-euler.sol.p0013 :refer [solve]]))

(deftest test-p0013
  (testing "The first ten digits."
    (is (= 5537376230 (solve 10)))))
