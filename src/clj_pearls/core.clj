(ns clj-pearls.core)


(defn separate
  "Single pass implementation of the juxtaposition of filter and remove, i.e., returns
  [(filter pred xs) (remove pred xs)]."
  ([pred xs] (separate pred xs [[] []]))
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


(defn join
  "Merges two surpasser count tables."
  ([txs tys] (join (count tys) txs tys))
  ([n txs tys]
   (cond
     (zero? n) txs
     (empty? txs) tys
     :else
     (let [[[x c] & txs*] txs
           [[y d] & tys*] tys]
       (if (< x y)
         (cons [x (+ c n)] (join n txs* tys))
         (cons [y d] (join (dec n) txs tys*)))))))

(defn table
  "Returns list of pairs of elements and surpasser counts for those elements,
  ordered by the elements. Such an ordered list is called a surpasser count
  table."
  [xs]
  {:pre [(not (empty? xs))]}
  (if (= (count xs) 1)
    (list [(first xs) 0])
    (let [m (count xs)
          n (quot m 2)
          [ys zs] (split-at n xs)]
      (join (- m n) (table ys) (table zs)))))

(defn maximum-surpasser-count
  "Returns the maximum surpasser count of the numerical list `xs`."
  [xs]
  {:pre [(not (empty? xs))]}
  (apply max (map second (table xs))))


(not (empty? []))
