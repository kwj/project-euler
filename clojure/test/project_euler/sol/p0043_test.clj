(ns project-euler.sol.p0043-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [project-euler.sol.p0043 :refer [solve]]))

(deftest test-p0043
  (testing "Problem 43."
    (is (= 16695334890 (solve)))))
