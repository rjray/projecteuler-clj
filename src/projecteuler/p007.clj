(ns projecteuler.core
  (:require [projecteuler.core :refer [prime?]]))

;; https://projecteuler.net/problem=7

(defn nth-prime [& [n]]
  (let [n (or n 10001)]
    (last
     (take (dec n)
           (filter prime? (take-nth 2 (range 1 Integer/MAX_VALUE)))))))
