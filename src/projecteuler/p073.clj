(ns projecteuler.p073
  (:require [clojure.math.numeric-tower :as math]))

;; https://projecteuler.net/problem=73

(defn- count-in-range [d]
  (let [top (int (math/ceil (/ d 2)))
        bot (int (math/floor (/ d 3)))]
    (count (filter #(= 1 (math/gcd % d)) (range (inc bot) top)))))

(defn counting-fractions-in-range [& [maxd]]
  (let [maxd (or maxd 12000)]
    (reduce + (pmap count-in-range (range 5 (inc maxd))))))
