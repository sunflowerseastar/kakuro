(ns kakuro.core-test
  (:require
   [cljs.test :refer-macros [deftest is testing run-tests]]
   [kakuro.utilities :refer [clue-square->clue-notation
                             possible-sum?
                             board->clue-notation
                             clue-notation->board
                             gen-board
                             get-square
                             get-right-clue
                             get-down-clue
                             get-num-entries-below
                             get-num-entries-right
                             board->entries]]))

(def simple-test-board [[{:type :black} {:type :clue, :x 1, :y 0, :clues {:down {:sum 4, :distance 2}}} {:type :clue, :x 2, :y 0, :clues {:down {:sum 6, :distance 2}}}] [{:type :clue, :x 0, :y 1, :clues {:right {:sum 3, :distance 2}}} {:type :entry, :x 1, :y 1} {:type :entry, :x 2, :y 1}] [{:type :clue, :x 0, :y 2, :clues {:right {:sum 7, :distance 2}}} {:type :entry, :x 1, :y 2} {:type :entry, :x 2, :y 2}]])
(def simple-test-clue-notation '([:d 1 0 4 2] [:d 2 0 6 2] [:r 0 1 3 2] [:r 0 2 7 2]))

(deftest test-possible-sum?
  (is (= (possible-sum? -1) false))
  (is (= (possible-sum? 0) false))
  (is (= (possible-sum? 1) true))
  (is (= (possible-sum? 19) true))
  (is (= (possible-sum? 45) true))
  (is (= (possible-sum? 46) false))
  (is (= (possible-sum? nil) false)))

(deftest test-clue-square->clue-notation
  (is (= (clue-square->clue-notation {:type :clue, :x 0, :y 1, :clues {:right {:sum 16, :distance 2}}}) '([:r 0 1 16 2])))
  (is (= (clue-square->clue-notation {:type :clue, :x 5, :y 5, :clues {:right {:sum 16, :distance 2} :down {:sum 11, :distance 3}}}) '([:r 5 5 16 2] [:d 5 5 11 3])))
  (is (= (clue-square->clue-notation {:type :clue, :x 1, :y 0, :clues {:down {:sum 4 :distance 2}}}) '([:d 1 0 4 2]))))

(deftest test-board->clue-notation
  (is (= (board->clue-notation simple-test-board) simple-test-clue-notation)))

(deftest test-gen-board
  (is (= (gen-board 3 3) [[{} {} {}] [{} {} {}] [{} {} {}]])))

(deftest test-clue-notation->board
  (is (= (clue-notation->board simple-test-clue-notation) simple-test-board)))
