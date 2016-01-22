(ns projecteuler.core
  (:require [projecteuler.core :refer [primes triangular-numbers]]))

;; https://projecteuler.net/problem=12

(defn- factorize [n]
  (loop [x n [p & ps] primes factors []]
    (cond (= 1 x) factors
          (zero? (mod x p)) (recur (/ x p) primes (conj factors p))
          :else (recur x ps factors))))
(defn- factorize-count [n]
  (reduce * (map (comp inc count) (vals (group-by identity (factorize n))))))
(defn highly-divisible-triangular-number [& [n]]
  (let [n (or n 500)]
    (first (drop-while #(< (factorize-count %) n) (triangular-numbers)))))
