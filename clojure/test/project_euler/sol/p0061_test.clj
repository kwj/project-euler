(ns project-euler.sol.p0061-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [project-euler.sol.p0061 :refer [solve]]))

(deftest test-p0061
  (testing "Up to pentagonal numbers."
    (is (= 19291 (solve 5))))
  (testing "Up to octagonal numbers."
    (is (= 28684 (solve 8)))))
