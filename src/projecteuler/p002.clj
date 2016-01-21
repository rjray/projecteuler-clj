(ns projecteuler.core)

;; https://projecteuler.net/problem=2

(defn sum-even-fibo-terms [& [maximum]]
  (let [maximum (or maximum 4000000)]
    (reduce + (filter even? (take-while #(< % maximum) fib-seq)))))
