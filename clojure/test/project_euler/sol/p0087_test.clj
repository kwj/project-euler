(ns project-euler.sol.p0087-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [project-euler.sol.p0087 :refer [solve]]))

(deftest test-p0087
  (testing "The upper is 50."
    (is (= 4 (solve 50))))
  (testing "The upper is 50000000."
    (is (= 1097343 (solve 50000000)))))
