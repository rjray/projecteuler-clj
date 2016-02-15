(ns projecteuler.core
  (:require [projecteuler.core :refer [corners-seq is-prime?]]))

;; https://projecteuler.net/problem=58

(defn spiral-primes [& [target]]
  (let [target (or target 0.1)]
    (loop [[c & cseq] (rest corners-seq)
           length     3
           prime      0
           total      1]
      (let [prime (+ prime (count (filter is-prime? c)))
            total (+ total 4)
            ratio (/ prime total)]
        (cond
         (< ratio target) (list length prime total)
         :else            (recur cseq (+ length 2) prime total))))))
