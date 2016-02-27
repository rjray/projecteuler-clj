(ns projecteuler.p035
  (:require [projecteuler.core :refer [primes]]))

;; https://projecteuler.net/problem=35

(defn- rotations [num]
  (let [strnum (str num)
        n      (count strnum)]
    (loop [m 1
           s strnum
           l (list num)]
      (cond
       (= m n) l
       :else
       (let [rotation (apply str (concat (rest s) (list (first s))))]
         (recur (inc m) rotation (cons (Integer/parseInt rotation) l)))))))
(defn- all-are-prime [c ps]
  (not (some nil? (map #(ps %) c))))
(defn- circular-primes [max]
  ;; Avg elapsed time: 44.71ms
  (let [primeset (set (take-while #(< % max) primes))]
    (filter #(all-are-prime (rotations %) primeset) primeset)))

(defn count-circular-primes [& [max]]
  (let [max (or max 1000000)]
    (count (circular-primes max))))
