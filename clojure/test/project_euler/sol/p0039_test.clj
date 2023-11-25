(ns project-euler.sol.p0039-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [project-euler.sol.p0039 :refer [solve]]))

(deftest test-p0039
  (testing "The limit is 1000."
    (is (= 840 (solve 1000)))))
