(ns projecteuler.core)

;; https://projecteuler.net/problem=20

(defn factorial-digit-sum [& [n]]
  (let [n (or n 100)
        v (! n)
        s (str v)]
    (reduce + (map num-map s))))
