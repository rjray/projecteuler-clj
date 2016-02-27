(ns projecteuler.p053
  (:require [projecteuler.core :refer [!]]))

;; https://projecteuler.net/problem=53

(defn- nCr [n r]
  (/ (! n) (*' (! r) (! (- n r)))))

(defn combinatoric-selections [& [maxn]]
  (let [maxn (or maxn 100)]
    (->> (for [n (range 23 (inc maxn)), r (range 1 (inc n))] (nCr n r))
         (filter #(> % 1000000))
         (count))))
