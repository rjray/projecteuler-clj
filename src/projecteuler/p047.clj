(ns projecteuler.core
  (:require [projecteuler.core :refer [factorize]]))

;; https://projecteuler.net/problem=47

(defn- distinct-prime-factors [n]
  (distinct (factorize n)))

(defn distinct-primes-factors-chain [l]
  (let [l (or l 4)]
    (loop [n 2, lst ()]
      (cond
       (= l (count lst))
           (reverse lst)
       (= l (count (distinct-prime-factors n)))
           (recur (inc n) (cons n lst))
       :else
           (recur (inc n) ())))))
