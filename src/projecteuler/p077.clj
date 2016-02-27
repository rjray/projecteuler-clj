(ns projecteuler.p077
  (:require [projecteuler.core :refer [primes]]))

;; https://projecteuler.net/problem=77

;; Attempt: Using dynamic programming

(defn- primes-to-n [n]
  (take-while #(< % n) primes))

(defn- calc-row [y nums matrix]
  (loop [x 1
         m matrix]
    (cond
     (= x (count nums))
         m
     (>= y (nums x))
         (recur (inc x)
                (assoc-in m [x y] (+ (get-in m [(dec x) y])
                                     (get-in m [x (- y (nums x))]))))
     :else
         (recur (inc x)
                (assoc-in m [x y] (get-in m [(dec x) y]))))))

(defn- count-summations [target]
  (let [nums   (vec (primes-to-n target))
        matrix (vec (cons (vec (map #(mod (inc %) 2) (range 0 (inc target))))
                          (vec (repeat (dec (count nums))
                                       (vec (repeat (inc target) 0))))))]
    (loop [sum    0
           matrix matrix]
      (cond
       (> sum target) (last (last matrix))
       :else (recur (inc sum) (calc-row sum nums matrix))))))

(defn prime-summations [& [target]]
  (let [target (or target 5000)]
    (->> (iterate inc 10)
         (map #(list % (count-summations %)))
         (filter #(> (second %) target))
         (first))))
