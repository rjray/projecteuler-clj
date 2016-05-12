(ns projecteuler.p075
  (:require [clojure.math.numeric-tower :as math]))

;; https://projecteuler.net/problem=75

;; Based on the algorithm from
;; http://www.mathblog.dk/project-euler-75-lengths-of-wire-right-angle-triangle/

(defn- get-counts [limit m n tv]
  (if (and (odd? (+ m n)) (= 1 (math/gcd n m)))
    (let [a (+ (* m m) (* n n))
          b (- (* m m) (* n n))
          c (* 2 m n)
          p (+ a b c)]
      (loop [pp p, tv tv]
        (cond
         (> pp limit) tv
         :else        (recur (+ pp p) (assoc tv pp (inc (tv pp)))))))
    tv))

(defn- find-counts [limit]
  (let [mlimit    (int (Math/sqrt (/ limit 2)))
        triangles (vec (repeat (inc limit) 0))]
    (loop [m 2, n 1, t triangles]
      (cond
       (= m mlimit) t
       (= n m)      (recur (inc m) 1 t)
       :else        (recur m (inc n) (get-counts limit m n t))))))

(defn singular-integer-right-triangles [& [maxp]]
  (let [maxp (or maxp 1500000)]
    (count (filter #(= % 1) (find-counts maxp)))))
