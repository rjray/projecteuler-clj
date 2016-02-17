(ns projecteuler.core
  (:require [projecteuler.core :refer [is-palindrome-num?]]))

;; https://projecteuler.net/problem=55

(defn- nreverse [n]
  (bigint (apply str (reverse (str n)))))

(defn- lychrel? [n]
  (loop [n n, iter 1]
    (let [new-n (+' n (nreverse n))]
      (cond
       (is-palindrome-num? new-n) false
       (> iter 50)            true
       :else                  (recur new-n (inc iter))))))

(defn lychrel-numbers [& [maxn]]
  (let [maxn (or maxn 10000)]
    (count (filter lychrel? (range 1 maxn)))))
