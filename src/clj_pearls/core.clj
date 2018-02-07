(ns clj-pearls.core)


(defn separate
  "Single pass implementation of the juxtaposition of filter and remove, i.e., returns
  [(filter pred xs) (remove pred xs)]."
  ([pred xs] (split pred xs [[] []]))
  ([pred [x & xs] [us vs]]
   (cond
     (nil? x) [us vs]
     (pred x) (recur pred xs [(conj us x) vs])
     :else (recur pred xs [us (conj vs x)]))))


(defn min-free
  "Returns the smallest number at least `a` that is not in `xs`. Assumes `xs` does
  not contain any duplicates. From Section 1 of the Pearls of Functional
  Algorithm Design."
  ([xs] (min-free 0 xs))
  ([a xs] (min-free a (count xs) xs))
  ([a n xs]
   (let [b (+ a 1 (quot n 2))
         [us vs] (separate (partial > b) xs)
         m (count us)]
     (cond
       (zero? n) a
       (= m (- b a)) (recur b (- n m) vs)
       :else (recur a m us)))))
