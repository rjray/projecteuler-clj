(ns projecteuler.p112
  (:require [projecteuler.core :refer [num-map]]))

;; https://projecteuler.net/problem=112

;; Just a brute-force approach for now

(defn- is-bouncy [n]
  (let [nlist (map num-map (str n))]
    (not (or (apply <= nlist) (apply >= nlist)))))

(defn bouncy-numbers [& [target]]
  (let [target (or target 99)]
    (loop [num 99, bouncy 0]
      (cond
       (>= (* bouncy 100) (* num target)) num
       :else         (recur (inc num)
                            (if (is-bouncy (inc num)) (inc bouncy) bouncy))))))
