(ns project-euler.sol.p0027-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [project-euler.sol.p0027 :refer [solve]]))

(deftest test-p0027
  (testing "abs(a) <= 1000 and abs(b) <= 1000."
    (is (= -59231 (solve)))))
