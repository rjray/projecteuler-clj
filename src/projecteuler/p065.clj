(ns projecteuler.core
  (:require [projecteuler.core :refer [num-map]]))

;; https://projecteuler.net/problem=65

;; This will generate the sequence of convergent factors needed for e:
(def ^:private e-convergent-seq
  (flatten (map #(list 1 (* % 2) 1) (iterate inc 1))))

(defn- e-terms [d n ecs]
  (cond
   (= n d) 0
   :else   (/ 1 (+ (ecs n) (e-terms d (inc n) ecs)))))

(defn convergents-of-e [& [term]]
  (let [term (or term 100)
        cseq (vec (take term e-convergent-seq))]
    (->> (+ 2 (e-terms (dec term) 0 cseq))
         (numerator)
         (str)
         (map num-map)
         (reduce +))))
