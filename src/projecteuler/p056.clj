(ns projecteuler.p056
  (:require [clojure.math.numeric-tower :as math]
            [projecteuler.core :refer [num-map]]))

;; https://projecteuler.net/problem=56

(defn- digit-sum [n]
  (reduce + (map num-map (str n))))

(defn powerful-digit-sum [& [maxval]]
  (let [maxval (or maxval 100)]
    (loop [a 2, b 1, cur 0]
      (cond
       (= a maxval) cur
       (= b maxval) (recur (inc a) 1 cur)
       :else        (recur a (inc b) (max cur (digit-sum (math/expt a b))))))))
