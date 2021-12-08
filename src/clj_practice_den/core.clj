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

; Computes the dot product of two sequences, assuming the vectors
; have the same length.
(defn dot-product
  [seq-a seq-b]
  (->> seq-a
       (map-indexed #(* %2 (get seq-b %1)))
       (reduce +)))

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

; Replicates each element of a sequence n times.
(defn replicate-each
  [coll n]
  (reduce #(into %1 (take n (repeat %2))) [] coll))

; Creates a list of integers in a range.
(defn x-range
  [lower-bound upper-bound]
  (let [coll-size (- upper-bound lower-bound)
        increment (fn [start-counter]
                    (let [state (atom (dec start-counter))]
                      #(swap! state inc)))]
    (repeatedly coll-size (increment lower-bound))))

; From 2 sequences, take the 1st item of each, then the 2nd & so on.
; Restrictions: interleave
(defn x-interleave
  [seq-a seq-b]
  (->> seq-a
       (keep-indexed
         (fn [index item-a]
           (when-let [item-b (get seq-b index)]
              (list item-a item-b))))
       flatten))

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
