(ns projecteuler.p010
  (:require [projecteuler.core :refer [prime?]]))

;; https://projecteuler.net/problem=10

(defn summation-of-primes [& [upper]]
  (let [upper (or upper 2000000)]
    (apply + (filter #(prime? % 10) (range 2 upper)))))
