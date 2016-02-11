(ns projecteuler.core)

;; https://projecteuler.net/problem=38

(defn- make-product [n x s]
  (cond
   (> (count s) 9) nil
   (< (count s) 9) (recur n (inc x) (str s (* x n)))
   :else           s))

(defn- is-pandigital? [s]
  (= (sort (seq s)) [\1 \2 \3 \4 \5 \6 \7 \8 \9]))

;; Avg elapsed time: 18.342ms (Linux/rjray)
(defn pandigital-multiples []
  (first (sort #(compare %2 %1)
               (filter is-pandigital?
                       (map #(make-product % 1 "") (range 1 10000))))))
