(ns clj-practice-den.core
  (:gen-class))

; Returns the last element in a sequence.
; Restrictions: last
(defn x-last
  [coll]
  (nth coll (dec (count coll))))

; Returns the nth element from a sequence.
; Restrictions: nth
(defn x-nth
  [coll n]
  (when (<= 0 n (dec (count coll)))
    (last (take (inc n) coll))))

; Returns the nth element from a sequence.
; Restrictions: Use state
(defn nth-v2
  [coll n]
  (when (<= 0 n (dec (count coll)))
    (let [ind (atom 0)]
      (some
        #(if (not= @ind n)
           (do (swap! ind inc) nil)
           %)
        coll))))

; Returns the nth element in a sequence.
; Restrictions: Use loop
(defn nth-v3
  [coll n]
  (loop [[item & remaining] coll
         ind 0]
    (if (= ind n)
      item
      (if (= ind (dec (count coll)))
        nil
        (recur remaining (inc ind))))))

; Keep every nth item of a sequence.
(defn keep-every-nth
  [coll n]
  (keep-indexed
    #(when (zero? (rem (inc %1) n)) %2)
    coll))

; Accepts a curried function of unknown arity n, returning an
; equivalent function of n arguments.
(defn de-curry
  [curried-fn]
  (fn [item & remaining]
    (let [curry (curried-fn item)]
      (if (fn? curry)
        (apply (de-curry curry) remaining)
        curry))))

; Restrictions: Take advantage of tail call optimization with loop/recur
(defn de-curry-v2
  [initial-curried-fn]
  (fn [& args]
    (loop [[item & remaining] args
           curried-fn initial-curried-fn]
      (if (empty? remaining)
        (curried-fn item)
        (recur remaining (curried-fn item))))))

; Flattens a sequence.
; Restrictions: flatten
(defn x-flatten
  [coll]
  (->> coll
       (tree-seq sequential? identity)
       rest
       (remove sequential?)))

; Remove consecutive duplicates from a sequence.
(defn remove-consecutive-dupes
  [coll]
  (reduce
    (fn [acc k]
      (if (= k (last acc))
        acc
        (conj acc k)))
    []
    coll))

; Remove consecutive duplicates from a sequence
; Restrictions: use partition-by
(defn remove-consecutive-dupes-v2
  [coll]
  (->> coll
       (partition-by identity)
       (map first)))

; Replicates each element of a sequence n times.
(defn replicate-each
  [coll n]
  (reduce #(into %1 (take n (repeat %2))) [] coll))

; Creates a list of integers in a range.
; Restrictions: range, iterate
(defn x-range
  [lower-bound upper-bound]
  (let [coll-size (- upper-bound lower-bound)
        increment (fn [start-counter]
                    (let [state (atom (dec start-counter))]
                      #(swap! state inc)))]
    (repeatedly coll-size (increment lower-bound))))

(defn x-range-v2
  [lower-bound upper-bound]
  (take (- upper-bound lower-bound) (iterate inc lower-bound)))

(defn- zippy
  [reducing-transform]
  (fn [seq-a seq-b]
    (loop [acc nil
           [item-a & remaining-a] seq-a
           [item-b & remaining-b] seq-b]
      (if (some nil? [item-a item-b])
        acc
        (recur
          (reducing-transform acc item-a item-b)
          remaining-a
          remaining-b)))))

; From 2 sequences, take the 1st item of each, then the 2nd & so on.
; Restrictions: interleave
(def x-interleave
  (zippy (fn [acc item-a item-b]
           (concat (or acc []) [item-a item-b]))))

; From 2 sequences, take the 1st item of each, then the 2nd & so on.
; The new form is represented as key-value pairs in a map.
; Restrictions: zipmap
(def x-zipmap
  (zippy (fn [acc item-a item-b]
           (assoc (or acc {}) item-a item-b))))

; Computes the dot product of two sequences, assuming the vectors
; have the same length.
(defn dot-product
  [seq-a seq-b]
  (->> seq-a
       (map-indexed #(* %2 (get seq-b %1)))
       (reduce +)))

(def dot-product-v2
  (zippy (fn [acc item-a item-b]
           (+ (or acc 0) (* item-a item-b)))))

; Calculate the factorial
(defn factorial
  [n]
  (->> n
       (iterate dec)
       (take n)
       (reduce *)))

; Split a sequence at n
; Restrictions: split-at
(def x-split-at (juxt take drop))

; Split a sequence into sub-sequences by type
(def split-by-type #(vals (group-by type %)))

; Create an infinite lazy sequence of x, (f x), (f (f x)), ...
; Restrictions: iterate
(defn x-iterate
  [value transform]
  (lazy-seq
    (cons
      value
      (x-iterate (transform value) transform))))

; Creates a lazy sequence of values transformed from an initial value
; by a variable number of transforms.  The transforms are applied in order
; and then repeated from the first transform.
; ex: lazy seq of x, (f1 x), (f2 (f1 x)), (f1 (f2 (f1 x)))...
(defn oscillating-iterate
  [value transform & remaining-transforms]
  (lazy-seq
    (cons
      value
      (apply
        oscillating-iterate
        (transform value)
        (concat remaining-transforms [transform])))))

; Compose fns right to left
; Restrictions: comp
(defn x-comp
  ([] identity)
  ([& fns]
   (fn [& args]
     (let [[f1 & fns] (reverse fns)]
       (reduce #(%2 %1) (apply f1 args) fns)))))
