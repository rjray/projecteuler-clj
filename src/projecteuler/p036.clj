(ns projecteuler.p036
  (:require [projecteuler.core :refer [is-palindrome-num?]]))

;; https://projecteuler.net/problem=36

(defn- is-palindrome-binary-num? [x]
  (let [x-str (Integer/toString x 2)
        x-seq (seq x-str)]
    (= x-seq (reverse x-seq))))
(defn double-base-palindromes [& [maximum]]
  (let [maximum (or maximum 1000000)]
    (reduce + (filter is-palindrome-binary-num?
                      (filter is-palindrome-num? (range 1 maximum))))))
