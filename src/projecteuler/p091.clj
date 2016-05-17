(ns projecteuler.p091
  (:require [clojure.math.numeric-tower :as math]))

;; https://projecteuler.net/problem=91

;; Based on the algorithm from
;; http://www.mathblog.dk/project-euler-91-right-angle-triangles-quadrant/

(defn- minval [x y size]
  (let [fact (math/gcd x y)]
    (* (min (int (/ (* y fact) x)) (int (/ (* (- size x) fact) y))) 2)))

(defn right-triangles-int [& [size]]
  (let [size (or size 50)]
    (loop [x 1, y 1, result (* size size 3)]
      (cond
       (> x size) result
       (> y size) (recur (inc x) 1 result)
       :else      (recur x (inc y) (+ result (minval x y size)))))))

(defn right-triangles-int2 [& [size]]
  (let [size (or size 50)]
    (reduce +
            (* size size 3)
            (for [x (range 1 (inc size)), y (range 1 (inc size))]
              (minval x y size)))))
