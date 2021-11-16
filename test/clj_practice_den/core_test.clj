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
    (are [expected seqA seqB]
         (= expected (dot-product seqA seqB))
         32 [1 2 3] [4 5 6]
         256 [2 5 6] [100 10 1])))

(deftest test-flat
  (testing "Flatten a sequence."
    (are [expected coll]
         (= expected (flat coll))
         '(1 2 3 4 5 6) '((1 2) 3 [4 [5 6]])
         '("a" "b" "c") ["a" ["b"] "c"]
          '(:a) '((((:a)))))))
