(ns projecteuler.core
  (:require [projecteuler.core :refer [totient]]))

;; https://projecteuler.net/problem=72

(defn counting-fractions [& [maxd]]
  (let [maxd (or maxd 1000000)]
    (reduce + (pmap totient (range 2 (inc maxd))))))
