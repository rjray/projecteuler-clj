(ns projecteuler.p022
  (:require [projecteuler.core :refer [word-score]]))

;; https://projecteuler.net/problem=22

(defn names-scores [& [file]]
  (let [file  (or file "resources/022.txt")
        data  (slurp file)
        names (sort (re-seq #"[A-Z]+" data))]
    (reduce + (map-indexed #(* (inc %1) (word-score %2)) names))))
