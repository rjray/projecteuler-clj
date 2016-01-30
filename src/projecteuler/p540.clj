(ns projecteuler.core
  (:require [clojure.math.numeric-tower :as math]))

;; https://projecteuler.net/problem=540

; NOT WORKING YET.
;
; Correct answer for P(20), incorrect for P(1000000). Dies with memory/GC error
; when run for P(3141592653589793N).

(defn- euclid-primitive [m n]
  (let [a (- (* m m) (* n n))
        b (* 2 m n)
        c (+ (* m m) (* n n))]
    (vec (sort (list a b c)))))
(defn- get-euclid-prims [max-p]
  (let [max-n (int (Math/sqrt (/ max-p 2)))
        max-m (inc max-n)]
    (loop [n 1, c #{}]
      (cond
       (> n max-n) c
       :else
       (recur (inc n)
              (set (concat c
                           (filter #(<= (last %) max-p)
                                   (map #(euclid-primitive % n)
                                        (filter #(= 1 (math/gcd % n))
                                                (range (inc n)
                                                       (inc max-m) 2)))))))))))
