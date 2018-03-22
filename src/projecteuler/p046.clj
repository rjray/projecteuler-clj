(ns projecteuler.p046
  (:require [projecteuler.core :refer [primes is-prime?]]))

;; https://projecteuler.net/problem=46

(defn- is-twice-square? [n]
  (let [m (/ n 2), mr (Math/sqrt m)]
    (== mr (int mr))))

(defn- goldbach? [n]
  (let [ps (take-while #(< % n) primes)]
    (loop [ps ps]
      (cond
        (empty? ps)       false
        (is-twice-square? (- n (first ps))) true
        :else             (recur (rest ps))))))

(defn goldbach-other []
  (first (remove goldbach?
                 (remove is-prime?
                         (filter odd? (iterate inc 3))))))
