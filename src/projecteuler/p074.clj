(ns projecteuler.p074
  (:require [projecteuler.core :refer [!]]))

;; https://projecteuler.net/problem=74

(defn- digit-fact-sum [n]
  (reduce + (map ! (map num-map (str n)))))

(defn- find-chain-length [n]
  (loop [num n, chain #{}]
    (cond
     (chain num) (list n (count chain))
     :else       (recur (digit-fact-sum num) (conj chain num)))))

(defn digit-factorial-chains [& [maxn length]]
  (let [maxn   (or maxn 1000000)
        length (or length 60)]
    (->> (range 1 maxn)
         (map find-chain-length)
         (filter #(= (second %) length))
         (count))))
