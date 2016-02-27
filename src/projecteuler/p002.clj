(ns projecteuler.p002
  (:require [projecteuler.core :refer [fib-seq]]))

;; https://projecteuler.net/problem=2

(defn sum-even-fibo-terms [& [maximum]]
  (let [maximum (or maximum 4000000)]
    (reduce + (filter even? (take-while #(< % maximum) fib-seq)))))
