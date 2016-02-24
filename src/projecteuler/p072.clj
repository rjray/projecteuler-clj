(ns projecteuler.core
  (:require [projecteuler.core :refer [factorize]]
            [clojure.math.numeric-tower :as math]))

;; https://projecteuler.net/problem=72

(defn- factorize-pairs [n]
  (map #(list (first %) (count (second %))) (group-by identity (factorize n))))

(defn- totient [n]
  (reduce * 1 (for [[p e] (factorize-pairs n)]
                (* (dec p) (math/expt p (dec e))))))

(defn counting-fractions [& [maxd]]
  (let [maxd (or maxd 1000000)]
    (reduce + (pmap totient (range 2 (inc maxd))))))
