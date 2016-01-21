(ns projecteuler.core)

;; https://projecteuler.net/problem=4

(defn largest-palindrome-product []
  (apply max
         (filter is-palindrome-num?
                 (for [a (range 101 1000)
                       b (range 101 1000)]
                   (* a b)))))
