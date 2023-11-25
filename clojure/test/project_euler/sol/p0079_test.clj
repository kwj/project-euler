(ns project-euler.sol.p0079-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [project-euler.sol.p0079 :refer [solve]]))

(deftest test-p0079
  (testing "Using 0079_keylog.txt."
    (is (= "73162890" (solve)))))
