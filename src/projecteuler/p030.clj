(ns projecteuler.core
  (:require [projecteuler.core :refer [num-map]]))

;; https://projecteuler.net/problem=30

(defn digit-nth-powers-sum-naive [& [n]]
  (let [n   (or n 5)
        pow (fn [a] (apply * (repeat n a)))
        sum (fn [a] (apply + (map pow (map num-map (str a)))))
        max (* (inc n) (pow 9))]
    (reduce + (filter #(= % (sum %)) (range 10 (inc max))))))
