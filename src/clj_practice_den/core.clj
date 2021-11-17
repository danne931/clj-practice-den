(ns clj-practice-den.core
  (:gen-class))

; Returns the last element in a sequence.
; Restrictions: last
(defn get-last
  [coll]
  (nth coll (dec (count coll))))

; Returns the nth element from a sequence.
; Restrictions: nth
(defn get-nth
  [coll n]
  (when (<= 0 n (dec (count coll)))
    (last (take (inc n) coll))))

; Returns the nth element from a sequence.
; Restrictions: nth, last, first, take, drop
(defn get-nth-v2
  [coll n]
  (when (<= 0 n (dec (count coll)))
    (let [ind (atom 0)]
      (some
        #(if (not= @ind n)
           (do (swap! ind inc) nil)
           %)
        coll))))

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
(defn flat
  [coll]
  (->> coll
       (tree-seq sequential? identity)
       rest
       (remove sequential?)))

; Computes the dot product of two sequences, assuming the vectors
; have the same length.
(defn dot-product
  [seqA seqB]
  (->> seqA
       (map-indexed #(* %2 (get seqB %1)))
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
(defn int-range
  [lower-bound upper-bound]
  (let [coll-size (- upper-bound lower-bound)
        increment (fn [start-counter]
                    (let [state (atom (dec start-counter))]
                      #(swap! state inc)))]
    (repeatedly coll-size (increment lower-bound))))
