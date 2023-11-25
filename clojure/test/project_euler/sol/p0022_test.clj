(ns project-euler.sol.p0022-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [project-euler.sol.p0022 :refer [solve]]))

(deftest test-p0022
  (testing "Using 0022_names.txt."
    (is (= 871198282 (solve)))))
