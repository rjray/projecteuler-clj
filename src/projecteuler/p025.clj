(ns projecteuler.core)

;; https://projecteuler.net/problem=25

(defn fibo-1000 []
  (first (first (drop-while #(< (second %) 1000)
                            (map-indexed #(list (inc %1)
                                                (count (str %2))) fib-seq)))))
