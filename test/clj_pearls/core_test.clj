(ns clj-pearls.core-test
  (:require [clojure.test :refer :all]
            [clj-pearls.core :as sut]
            [clojure.test.check :refer [quick-check]]
            [clojure.test.check.properties :refer [for-all]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.clojure-test :refer [defspec]]))


(defspec min-free-returns-puncture-in-shuffled-punctured-sequence
  100
  (for-all [n gen/pos-int]
      (for-all [v (gen/choose 0 (dec n))]
          (= v (sut/min-free (shuffle (remove #(= v %) (range 0 n))))))))


(defspec separate-is-equivalent-to-juxt-filter-remove
  100
  (for-all [xs (gen/vector gen/int)
            t gen/int]
      (let [pred (partial < t)]
        (= [(filter pred xs) (remove pred xs)] (sut/separate pred xs)))))


(deftest maximum-surpasser-count-test
  (testing "GENERATING example from the book"
    (is (= 6 (sut/maximum-surpasser-count (map int "GENERATING"))))))
