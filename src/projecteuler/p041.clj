(ns projecteuler.p041
  (:require [projecteuler.core :refer [prime?]]
            [clojure.math.combinatorics :as comb]))

;; https://projecteuler.net/problem=41

(defn- digit-permutations [from to]
  (apply concat (map comb/permutations (map #(range 1 (inc %))
                                            (range from (inc to))))))
(defn- permutation-to-num [p]
  (Integer/parseInt (reduce str p)))

(defn largest-pandigital-prime []
  (first (filter prime? (map permutation-to-num
                             (reverse (digit-permutations 4 9))))))
