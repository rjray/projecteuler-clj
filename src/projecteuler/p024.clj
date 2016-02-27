(ns projecteuler.p024
  (:require [clojure.math.combinatorics :as comb])

;; https://projecteuler.net/problem=24

(defn lexicographic-permutations [& [n]]
  (let [n (or n 1000000)]
    (->> (range 10)
         (comb/permutations)
         (drop (dec n))
         (first)
         (apply str))))
