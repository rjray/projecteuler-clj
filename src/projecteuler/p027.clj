(ns projecteuler.core
  (:require [projecteuler.core :refer [is-prime?]])
  )

;; https://projecteuler.net/problem=27

(defn- quadratic [n a b]
  (+ (* n n) (* a n) b))
(defn- prime-list [a b]
  (take-while is-prime? (map #(quadratic % a b) (iterate inc 0))))
(defn quadratic-primes-prod []
  (second (first (sort #(compare (first %2) (first %1))
                       (for [a (range -999 1000), b (range -999 1000)]
                         (list (count (prime-list a b)) (* a b)))))))
