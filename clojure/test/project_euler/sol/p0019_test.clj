(ns project-euler.sol.p0019-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [project-euler.sol.p0019 :refer [solve]]))

(deftest test-p0019
  (testing "Number of Sundays from 1 Jan 1901 to 31 Dec 2000."
    (is (= 171 (solve)))))
