(ns projecteuler.p025
  (:require [projecteuler.core :refer [fib-seq]]))

;; https://projecteuler.net/problem=25

(defn fibo-1000 []
  (ffirst (drop-while #(< (second %) 1000)
                      (map-indexed #(list (inc %1)
                                          (count (str %2))) fib-seq))))
