(ns projecteuler.p004
  (:require [projecteuler.core :refer [is-palindrome-num?]]))

;; https://projecteuler.net/problem=4

(defn largest-palindrome-product []
  (apply max
         (filter is-palindrome-num?
                 (for [a (range 101 1000)
                       b (range 101 1000)]
                   (* a b)))))
