(ns projecteuler.p050
  (:require [projecteuler.core :refer [primes is-prime?]]))

;; https://projecteuler.net/problem=50

(def ^:private prime-sums
  (map first
       (iterate (fn [[sum s]] [(+ sum (first s)) (rest s)])
                [0 primes])))

(defn consecutive-prime-sums [& [maxval]]
  (let [maxval (or maxval 1000000)]
    (loop [c 1]
      (let [sums (reverse (take c prime-sums))
            subs (take c (reverse (take-while #(< (- % (last sums)) maxval)
                                              (rest prime-sums))))]
        (if-let [el (some #(when (prime? %) %)
                          (map - subs sums))]
          el
          (recur (inc c)))))))
