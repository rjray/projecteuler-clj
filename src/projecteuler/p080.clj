(ns projecteuler.p080
  (:require [clojure.math.numeric-tower :as math]
            [projecteuler.core :refer [num-map digit-sum]]))

;; https://projecteuler.net/problem=80

;; Based on the C# algorithm here:
;; http://www.mathblog.dk/project-euler-80-digits-irrational-square-roots/
;; which uses an algorithm taken from this paper:
;; http://www.afjarvis.staff.shef.ac.uk/maths/jarvisspec02.pdf

(defn- squares-set [top]
  (set (take-while #(<= % top) (map #(* % %) (iterate inc 1)))))

(defn- squareroot [n digits]
  (let [limit (math/expt 10 (inc digits))
        a     (bigint (* 5 n))
        b     (bigint 5)]
    (loop [a a, b b]
      (cond
       (>= b limit) (bigint (/ b 10))
       (>= a b)     (recur (- a b) (+ b 10))
       :else        (recur (* a 100) (+ 5 (* 100 (bigint (/ b 10)))))))))

(defn square-root-digits-sum [& [top digits]]
  (let [top       (or top 100)
        digits    (or digits 100)
        squareset (squares-set top)]
    (apply + (map digit-sum
                  (map #(squareroot % digits)
                       (remove squares (range 1 (inc top))))))))
