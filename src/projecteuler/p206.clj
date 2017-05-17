(ns projecteuler.p206
  (:require [projecteuler.core :refer [num-map]]))

;; https://projecteuler.net/problem=206

;; Based on the algorithm here:
;; https://blog.dreamshire.com/project-euler-206-solution/

(defn- match-digit [str x]
  (= (inc x) (num-map (nth str (* x 2)))))

(defn- is-solution [square]
  (let [str (str square)]
    (every? #(match-digit str %) (range 9))))

(defn concealed-square []
  (loop [n 138902663] ; sqrt(19293949596979899)
    (cond
     (is-solution (* n n)) (* n 10)
     :else                 (recur (- n 2)))))
