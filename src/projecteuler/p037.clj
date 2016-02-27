(ns projecteuler.p037
  (:require [projecteuler.core :refer [is-prime? primes]]
            [clojure.math.numeric-tower :as math]))

;; https://projecteuler.net/problem=37

(defn- l2r-truncatable? [num]
  (let [scale (int (Math/log10 num))]
    (loop [n (rem num (math/expt 10 scale)), p (dec scale)]
      (cond
       (zero? p) (is-prime? n)
       (not (is-prime? n)) false
       :else
       (recur (rem n (math/expt 10 p)) (dec p))))))

(defn- r2l-truncatable? [num]
  (let [scale (int (Math/log10 num))]
    (loop [n (quot num 10), p 1]
      (cond
       (= p scale) (is-prime? n)
       (not (is-prime? n)) false
       :else
       (recur (quot n 10) (inc p))))))

;; Avg elapsed time: 16774.54ms (Linux/rjray)
(defn sum-truncatable-primes []
  (reduce + (take 11 (filter #(and (l2r-truncatable? %)
                                   (r2l-truncatable? %)) (drop 4 primes)))))
