(ns project-euler.sol.p0098-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [project-euler.sol.p0098 :refer [solve]]))

(deftest test-p0098
  (testing "Using 0098_words.txt."
    (is (= 18769 (solve)))))
