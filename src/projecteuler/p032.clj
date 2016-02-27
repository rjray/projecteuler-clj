(ns projecteuler.p032
  (:require [clojure.math.combinatorics :as comb]))

;; https://projecteuler.net/problem=32

(defn- is-valid? [[i1 i2 prod]]
  (= (* i1 i2) prod))

(defn- valid-products [[d1 d2 d3 d4 d5 d6 d7 d8 d9]]
  (let [sets (list
              (list (+ (* d1 10) d2)
                    (+ (* 100 d3) (* 10 d4) d5)
                    (+ (* 1000 d6) (* 100 d7) (* 10 d8) d9))
              (list d1
                    (+ (* 1000 d2) (* 100 d3) (* 10 d4) d5)
                    (+ (* 1000 d6) (* 100 d7) (* 10 d8) d9)))]
    (map last (filter is-valid? sets))))

(defn- gather-valid []
  (flatten (map valid-products (comb/permutations (range 1 10)))))

(defn sum-pandigital-products []
  (reduce + (distinct (gather-valid))))
