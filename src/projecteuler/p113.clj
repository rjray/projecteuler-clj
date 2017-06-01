(ns projecteuler.p113
  (:require [projecteuler.core :refer [!]]))

;; https://projecteuler.net/problem=113

;; Based on the analysis here:
;; http://www.mathblog.dk/project-euler-113-googol-not-bouncy/

(defn- choose [n k]
  (/ (! n) (* (! (- n k)) (! k))))

(defn non-bouncy-numbers [& [n]]
  (let [n (or n 100)]
    (+ (choose (+ n 10) 10) (choose (+ n 9) 9) -2 (* -10 n))))
