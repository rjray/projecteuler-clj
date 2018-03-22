(ns projecteuler.p143
  (:require [clojure.math.numeric-tower :as math]
            [clojure.set :as sets]))

;; https://projecteuler.net/problem=143

;; Based on the analysis and algorithm here:
;; http://www.mathblog.dk/project-euler-143-investigating-the-torricelli-point-of-a-triangle/

(defn- k-loop [limit a b pairs]
  (loop [k 1, pairs pairs]
    (let [ka (* k a), kb (* k b)]
      (cond
       (>= (+ ka kb) limit) pairs
       :else
       (recur (inc k) (conj pairs (list ka kb) (list kb ka)))))))

(defn- v-loop [limit u pairs]
  (loop [v 1, pairs pairs]
    (cond
      (= u v)                 pairs
      (not= (math/gcd u v) 1) (recur (inc v) pairs)
      (zero? (mod (- u v) 3)) (recur (inc v) pairs)
      :else
      (let [a (+ (* 2 u v) (* v v)), b (- (* u u) (* v v))]
        (if (> (+ a b) limit)
          pairs
          (recur (inc v) (k-loop limit a b pairs)))))))

(defn- get-pairs [limit]
  (let [limit-sqrt (inc (int (Math/sqrt limit)))]
    (loop [u 1, pairs #{}]
      (cond
       (= u limit-sqrt) pairs
       :else            (recur (inc u) (v-loop limit u pairs))))))

(defn- get-pair-sums [pair idx-pairs limit]
  (let [a  (first pair)
        b  (second pair)
        sa (set (map second (idx-pairs a)))
        sb (set (map second (idx-pairs b)))]
    (filter #(< % limit)
            (map #(+ a b %) (sets/intersection sa sb)))))

(defn torricelli-points [& [limit]]
  (let [limit     (or limit 120000)
        pairs     (get-pairs limit)
        idx-pairs (group-by first pairs)]
    (apply + (set (flatten (map #(get-pair-sums % idx-pairs limit) pairs))))))
