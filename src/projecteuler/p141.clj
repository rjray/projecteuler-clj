(ns projecteuler.p141
  (:require [clojure.math.numeric-tower :as math]))

;; https://projecteuler.net/problem=141

;; Based on the analysis and algorithm from
;; http://www.mathblog.dk/project-euler-141investigating-progressive-numbers-n-which-are-also-square/

(defn- get-progressive-squares [limit]
  (let [alimit (int (+ (Math/pow limit (/ 1 3)) 0.00001))]
    (loop [a 2, b 1, c 1, ps #{}]
      (let [a3 (* a a a), b2 (* b b)]
        (cond
         (= a alimit)         ps
         (= b a)              (recur (inc a) 1 1 ps)
         (> (math/gcd a b) 1) (recur a (inc b) 1 ps)
         (>= (+ b2 (* a3 b2))
             limit)           (recur (inc a) 1 1 ps)
         :else
         (let [n     (+ (* a3 b c c) (* b2 c))
               sqrtn (int (Math/sqrt n))]
           (cond
            (>= n limit)          (recur a (inc b) 1 ps)
            (= n (* sqrtn sqrtn)) (recur a b (inc c) (conj ps n))
            :else                 (recur a b (inc c) ps))))))))

(defn progressive-squares [& [limit]]
  (->> (or limit 1000000000000)
       (get-progressive-squares)
       (apply +')))
