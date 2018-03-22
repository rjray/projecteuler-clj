(ns projecteuler.p016
  (:require [projecteuler.core :refer [num-map]]))

;; https://projecteuler.net/problem=16

(defn power-digit-sum [& [x n]]
  (let [x (or x 2)
        n (or n 1000)]
    (reduce + (map num-map (str (.pow (BigInteger. (str x)) n))))))
