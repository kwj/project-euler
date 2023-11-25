(ns project-euler.sol.p0068-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [project-euler.sol.p0068 :refer [solve]]))

(deftest test-p0068
  (testing "The 3-gon ring."
    (is (= 432621513 (solve 3))))
  (testing "The 5-gon ring."
    (is (= 6531031914842725 (solve 5)))))
