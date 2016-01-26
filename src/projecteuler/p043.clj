(ns projecteuler.core
  (:require [projecteuler.core :refer [primes]]
            [clojure.math.combinatorics :as comb]))

;; https://projecteuler.net/problem=43

(def offset-prime-map (zipmap (range 2 9) (take 7 primes)))

(defn- is-divisible? [numstr]
  (not (some #(= % false)
             (map #(zero? (mod (Integer/parseInt (subs numstr (dec %) (+ % 2)))
                               (offset-prime-map %))) (keys offset-prime-map)))))
(defn sub-string-divisible-sum []
  (apply + (map bigint
                (filter is-divisible?
                        (map #(apply str %) (comb/permutations (range 10)))))))
