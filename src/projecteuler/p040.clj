(ns projecteuler.p040
  (:require [projecteuler.core :refer [num-map]]))

;; https://projecteuler.net/problem=40

(defn- pow-10 [n]
  (reduce * (repeat n 10)))
(defn- get-digit [n]
  (loop [n n, s 1]
    (cond
     (> n (count (str s))) (recur (- n (count (str s))) (inc s))
     :else (num-map (nth (str s) (dec n))))))

(defn champernowne []
  (reduce * (map #(get-digit (pow-10 %)) (range 7))))
