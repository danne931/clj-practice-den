(ns clj-practice-den.core-test
  (:require [clojure.test :refer :all]
            [clj-practice-den.core :as core]))

(deftest test-last
  (testing "Get the last element of a sequence."
    (are [expected coll]
         (= expected (core/x-last coll))
         5 [1 2 3 4 5]
         3 '(5 4 3)
         "d" ["b" "c" "d"])))

(deftest test-nth
  (testing "Get the nth element of a sequence."
    (are [expected coll n]
         (= expected (core/x-nth coll n))
         6 '(4 5 6 7) 2
         :a [:a :b :c] 0
         2 [1 2 3 4] 1
         [5 6] '([1 2] [3 4] [5 6]) 2)))

(deftest test-nth-v2
  (testing "Get the nth element of a sequence. Implementation (2)"
    (are [expected coll n]
         (= expected (core/nth-v2 coll n))
         6 '(4 5 6 7) 2
         :a [:a :b :c] 0
         2 [1 2 3 4] 1
         [5 6] '([1 2] [3 4] [5 6]) 2)))

(deftest test-nth-v3
  (testing "Get the nth element of a sequence. Implementation (3)"
    (are [expected coll n]
         (= expected (core/nth-v3 coll n))
         6 '(4 5 6 7) 2
         :a [:a :b :c] 0
         2 [1 2 3 4] 1
         [5 6] '([1 2] [3 4] [5 6]) 2)))

(deftest test-de-curry
  (testing "Flatten the curry."
    (is (= 25
           ((core/de-curry
              (fn [a]
                (fn [b]
                  (* a b))))
            5 5)))
    (is (= 10
           ((core/de-curry
              (fn [a]
                (fn [b]
                  (fn [c]
                    (fn [d]
                      (+ a b c d))))))
            1 2 3 4)))))

(deftest test-de-curry-v2
  (testing "Flatten the curry. Implemention (2) loop/recur"
    (is (= 25
           ((core/de-curry-v2
              (fn [a]
                (fn [b]
                  (* a b))))
            5 5)))
    (is (= 10
           ((core/de-curry-v2
              (fn [a]
                (fn [b]
                  (fn [c]
                    (fn [d]
                      (+ a b c d))))))
            1 2 3 4)))))

(deftest test-dot-product
  (testing "Compute the dot product of 2 sequences."
    (are [expected seq-a seq-b]
         (= expected (core/dot-product seq-a seq-b))
         32 [1 2 3] [4 5 6]
         256 [2 5 6] [100 10 1])))

(deftest test-flatten
  (testing "Flatten a sequence."
    (are [expected coll]
         (= expected (core/x-flatten coll))
         '(1 2 3 4 5 6) '((1 2) 3 [4 [5 6]])
         '("a" "b" "c") ["a" ["b"] "c"]
          '(:a) '((((:a)))))))

(deftest test-remove-consecutive-dupes
  (testing "Remove consecutive dupes in a sequence."
    (are [expected coll]
         (= expected (core/remove-consecutive-dupes coll))
         [1 2 3 4] [1 1 1 2 3 3 4 4]
         '([1 2] [3 4] 5 6) '([1 2] [1 2] [1 2] [3 4] [3 4] 5 5 6)))
  (testing "Remove consecutive dupes works for strings."
    (is (= "Dan"
           (apply str (core/remove-consecutive-dupes "DDDaaaaannnn"))))))

(deftest test-replicate-each
  (testing "Replicate each element n times."
    (are [expected coll n]
         (= expected (core/replicate-each coll n))
         [1 1 1 2 2 2] [1 2] 3
         [1 2 3] [1 2 3] 1
         [["a" "b"] ["a" "b"] ["c" "d"] ["c" "d"]] [["a" "b"] ["c" "d"]] 2)))

(deftest test-range
  (testing "Create a list of integers in a range."
    (are [expected lower-bound upper-bound]
         (= expected (core/x-range lower-bound upper-bound))
         [-2 -1 0 1] -2 2
         [0 1 2 3] 0 4)))

(deftest test-interleave
  (testing "Interleave 2 sequences."
    (are [expected seq-a seq-b]
         (= expected (core/x-interleave seq-a seq-b))
         [1 3 2 4] [1 2] [3 4]
         [1 3 2 4] [1 2] [3 4 5 6]
         [1 3] [1] [3 4 5 6]
         [1 3] [1 2] [3])))

(deftest test-keep-every-nth
  (testing "Keep every nth element of a sequence."
    (are [expected coll n]
         (= expected (core/keep-every-nth coll n))
         [5 7] '(4 5 6 7) 2
         [:c :f :i] [:a :b :c :d :e :f :g :h :i] 3)))

(deftest test-factorial
  (testing "Calculate the factorial."
    (are [expected n]
         (= expected (core/factorial n))
         1 0
         1 1
         6 3
         24 4
         120 5)))
