(ns clj-practice-den.core-test
  (:require [clojure.test :refer :all]
            [clj-practice-den.core :refer :all]))

(deftest test-get-last
  (testing "Get the last element of a sequence."
    (are [expected coll]
         (= expected (get-last coll))
         5 [1 2 3 4 5]
         3 '(5 4 3)
         "d" ["b" "c" "d"])))

(deftest test-get-nth
  (testing "Get the nth element of a sequence."
    (are [expected coll n]
         (= expected (get-nth coll n))
         6 '(4 5 6 7) 2
         :a [:a :b :c] 0
         2 [1 2 3 4] 1
         [5 6] '([1 2] [3 4] [5 6]) 2)))

(deftest test-get-nth-v2
  (testing "Get the nth element of a sequence."
    (are [expected coll n]
         (= expected (get-nth-v2 coll n))
         6 '(4 5 6 7) 2
         :a [:a :b :c] 0
         2 [1 2 3 4] 1
         [5 6] '([1 2] [3 4] [5 6]) 2)))

(deftest test-de-curry
  (testing "Flatten the curry."
    (is (= 25
           ((de-curry
              (fn [a]
                (fn [b]
                  (* a b))))
            5 5)))
    (is (= 10
           ((de-curry
              (fn [a]
                (fn [b]
                  (fn [c]
                    (fn [d]
                      (+ a b c d))))))
            1 2 3 4)))))

(deftest test-dot-product
  (testing "Compute the dot product of 2 sequences."
    (are [expected seq-a seq-b]
         (= expected (dot-product seq-a seq-b))
         32 [1 2 3] [4 5 6]
         256 [2 5 6] [100 10 1])))

(deftest test-flat
  (testing "Flatten a sequence."
    (are [expected coll]
         (= expected (flat coll))
         '(1 2 3 4 5 6) '((1 2) 3 [4 [5 6]])
         '("a" "b" "c") ["a" ["b"] "c"]
          '(:a) '((((:a)))))))

(deftest test-remove-consecutive-dupes
  (testing "Remove consecutive dupes in a sequence."
    (are [expected coll]
         (= expected (remove-consecutive-dupes coll))
         [1 2 3 4] [1 1 1 2 3 3 4 4]
         '([1 2] [3 4] 5 6) '([1 2] [1 2] [1 2] [3 4] [3 4] 5 5 6)))
  (testing "Remove consecutive dupes works for strings."
    (is (= "Dan"
           (apply str (remove-consecutive-dupes "DDDaaaaannnn"))))))

(deftest test-replicate-each
  (testing "Replicate each element n times."
    (are [expected coll n]
         (= expected (replicate-each coll n))
         [1 1 1 2 2 2] [1 2] 3
         [1 2 3] [1 2 3] 1
         [["a" "b"] ["a" "b"] ["c" "d"] ["c" "d"]] [["a" "b"] ["c" "d"]] 2)))

(deftest test-int-range
  (testing "Create a list of integers in a range."
    (are [expected lower-bound upper-bound]
         (= expected (int-range lower-bound upper-bound))
         [-2 -1 0 1] -2 2
         [0 1 2 3] 0 4)))

(deftest test-core-interleave
  (testing "Interleave 2 sequences."
    (are [expected seq-a seq-b]
         (= expected (core-interleave seq-a seq-b))
         [1 3 2 4] [1 2] [3 4]
         [1 3 2 4] [1 2] [3 4 5 6]
         [1 3] [1] [3 4 5 6]
         [1 3] [1 2] [3])))

(deftest test-keep-every-nth
  (testing "Keep every nth element of a sequence."
    (are [expected coll n]
         (= expected (keep-every-nth coll n))
         [5 7] '(4 5 6 7) 2
         [:c :f :i] [:a :b :c :d :e :f :g :h :i] 3)))
