(ns projecteuler.p023
  (:require [projecteuler.core :refer [sum-proper-divisors]]))

;; https://projecteuler.net/problem=23

(defn- abundant? [n]
  (< n (sum-proper-divisors n)))

(defn- abundant-sum? [n abundant]
  (some #(abundant (- n %)) (take-while #(< % n) abundant)))

;; Avg elapsed time: 5046ms
(defn sum-non-abundant-sums []
  (let [abundant (into (sorted-set) (filter abundant? (range 12 28124)))]
    (reduce + (remove #(abundant-sum? % abundant) (range 1 28124)))))
