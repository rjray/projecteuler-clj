(ns projecteuler.p102
  (:require [clojure.string :as str]))

;; https://projecteuler.net/problem=102

(defn- vec- [v1 v2]
  [(- (first v1) (first v2)) (- (second v1) (second v2))])

(defn- dot [v1 v2]
  (+ (* (first v1) (first v2)) (* (second v1) (second v2))))

;; This uses barycentric coordinates. See
;; http://www.blackpawn.com/texts/pointinpoly/
(defn- contains-origin? [[A B C]]
  (let [v0    (vec- C A)
        v1    (vec- B A)
        v2    (vec- [0 0] A)
        dot00 (dot v0 v0)
        dot01 (dot v0 v1)
        dot02 (dot v0 v2)
        dot11 (dot v1 v1)
        dot12 (dot v1 v2)
        invD  (/ 1 (- (* dot00 dot11) (* dot01 dot01)))
        u     (* invD (- (* dot11 dot02) (* dot01 dot12)))
        v     (* invD (- (* dot00 dot12) (* dot01 dot02)))]
    (and (>= u 0) (>= v 0) (< (+ u v) 1))))

(defn- line-to-vec [line]
  (->> (str/split line #",")
       (map #(Integer/parseInt %))
       (partition 2)
       (map vec)))

(defn triangle-containment [& [file]]
  (let [file  (or file "data/102.txt")
        data  (slurp file)]
    (->> (str/split data #"\n")
         (map line-to-vec)
         (filter contains-origin?)
         (count))))
