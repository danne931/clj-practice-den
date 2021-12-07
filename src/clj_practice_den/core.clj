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
; Restrictions: nth, last, first, take, drop
(defn nth-v2
  [coll n]
  (when (<= 0 n (dec (count coll)))
    (let [ind (atom 0)]
      (some
        #(if (not= @ind n)
           (do (swap! ind inc) nil)
           %)
        coll))))

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
  (fn [& args]
    (let [curry (curried-fn (first args))]
      (if (fn? curry)
        (apply (de-curry curry) (rest args))
        curry))))

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
       (map-indexed
         (fn [index item-a]
           (if-let [item-b (get seq-b index)]
              (list item-a item-b)
              [])))
       flatten))
