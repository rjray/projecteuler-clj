(ns projecteuler.p540
  (:require [clojure.math.numeric-tower :as math]))

;; https://projecteuler.net/problem=540

; NOT WORKING YET.
;
; Correct answer for P(20), correct for P(1000000). Runs too long when run for
; P(3141592653589793N).

(defn- valid-primitive? [m n p]
  (let [c (+ (* m m) (* n n))]
    (if (<= c p) 1 0)))
(defn- count-primitives [n max-m p]
  (reduce + (map #(valid-primitive? % n p)
                 (filter #(= 1 (math/gcd % n)) (range (inc n) (inc max-m) 2)))))

(defn count-primitive-pyth-triples [& [max]]
  (let [max-p (or max 3141592653589793N)
        max-n (int (Math/sqrt max-p))
        max-m max-n]
    (apply + (pmap #(count-primitives % max-m max-p) (range 1 (inc max-n))))))
