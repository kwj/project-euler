(ns project-euler.sol.p0059-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [project-euler.sol.p0059 :refer [solve]]))

(deftest test-p0059
  (testing "Using 0059_cipher.txt."
    (is (= 129448 (solve)))))
