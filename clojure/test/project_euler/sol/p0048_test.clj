(ns project-euler.sol.p0048-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [project-euler.sol.p0048 :refer [solve]]))

(deftest test-p0048
  (testing "Exponent is 10."
    (is (= "0405071317" (solve 10))))
  (testing "Exponent is 1000."
    (is (= "9110846700" (solve 1000)))))
