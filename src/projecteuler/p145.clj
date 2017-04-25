(ns projecteuler.p145
  (:require [projecteuler.core :refer [num-map]]))

;; https://projecteuler.net/problem=145

;; Just a basic brute-force approach with some optimization.

(defn- reverse-num [n]
  (Integer/parseInt (apply str (reverse (str n)))))

(defn- all-digits-odd? [n]
  (not-any? even? (map num-map (str n))))

(defn- leading-digit-even? [n]
  (re-find #"^[2468]" (str n)))

(defn reversible-numbers [& [maxn]]
  (let [maxn (or maxn 1000000000)]
    (* 2 (count (filter all-digits-odd?
                        (map #(+ % (reverse-num %))
                             (filter leading-digit-even?
                                     (range 21 maxn 2))))))))
