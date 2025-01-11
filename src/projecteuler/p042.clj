(ns projecteuler.p042
  (:require [projecteuler.core :refer [word-score triangular-numbers]]))

;; https://projecteuler.net/problem=42

(defn coded-triangle-numbers [& [file]]
  (let [file      (or file "resources/042.txt")
        data      (slurp file)
        words     (re-seq #"\w+" data)
        maxlen    (apply max (map count words))
        maxcode   (* maxlen 26)
        triangles (set (take-while #(< % maxcode) (triangular-numbers)))]
    (apply + (map #(if (nil? (triangles %)) 0 1) (map word-score words)))))
