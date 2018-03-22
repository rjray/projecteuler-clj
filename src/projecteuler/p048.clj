(ns projecteuler.p048
  (:require [clojure.string :as str]))

;; https://projecteuler.net/problem=48

(defn pow' [a]
  (reduce *' (repeat a a)))

(defn self-powers [& [max]]
  (let [max (or max 1000)]
    (reverse (take 10
                   (reverse (str (reduce +'
                                         (map pow'
                                              (range 1
                                                     (inc max))))))))))
