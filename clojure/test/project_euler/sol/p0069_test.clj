(ns project-euler.sol.p0069-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [project-euler.sol.p0069 :refer [solve]]))

(deftest test-p0069
  (testing "The upper is 10."
    (is (= 6 (solve 10))))
  (testing "The upper is 1000000."
    (is (= 510510 (solve 1000000)))))
