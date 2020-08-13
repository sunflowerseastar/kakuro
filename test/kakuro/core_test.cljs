(ns kakuro.core-test
  (:require
   [cljs.test :refer-macros [deftest is testing run-tests]]
   [kakuro.utilities :refer [clue-square->clue-notation]]))

(deftest test-clue-square->clue-notation
  (is (= (clue-square->clue-notation {:type :clue, :x 0, :y 1, :clues {:right {:sum 16, :distance 2}}})
         '([:r 0 1 16 2]))))
