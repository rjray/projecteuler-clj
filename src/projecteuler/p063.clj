(ns projecteuler.core
  (:require [clojure.math.numeric-tower :as math]))

;; https://projecteuler.net/problem=63

(defn- counts-for-n [n]
  (loop [a 1, acc 0]
    (let [b (math/expt a n)
          l (inc (int (Math/log10 b)))]
      (cond
       (> l n) acc
       (= l n) (recur (inc a) (inc acc))
       :else   (recur (inc a) acc)))))

(defn powerful-digit-counts []
  (loop [n 1, total 0]
    (let [c (counts-for-n n)]
      (cond
       (zero? c) total
       :else     (recur (inc n) (+ total c))))))
