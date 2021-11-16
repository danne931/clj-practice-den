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
