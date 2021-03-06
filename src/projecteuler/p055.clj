(ns projecteuler.p055
  (:require [projecteuler.core :refer [is-palindrome-num?]]
            [clojure.string :as str]))

;; https://projecteuler.net/problem=55

(defn- nreverse [n]
  (bigint (str/reverse (str n))))

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
