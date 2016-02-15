(ns projecteuler.core
  (:require [projecteuler.core :refer [corners-seq]]))

;; https://projecteuler.net/problem=28

(defn number-spiral-diagonals [& [size]]
  (let [s (or size 1001)
        c (/ (inc s) 2)]
    (if (even? s)
      nil
      (apply + (flatten (take c corners-seq))))))
