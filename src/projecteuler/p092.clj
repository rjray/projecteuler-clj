(ns projecteuler.core
  (:require [projecteuler.core :refer [num-map]]
            [clojure.set :as sets]))

;; https://projecteuler.net/problem=92

(def ^:private squares [0 1 4 9 16 25 36 49 64 81])

(defn- chain-next [n]
  (apply + (map squares (map num-map (str n)))))

(defn- get-chain [n set1 set89]
  (loop [n     n
         next  (chain-next n)
         chain (list n)]
    (cond
     (set1 n)  (cons 1 chain)
     (set89 n) (cons 89 chain)
     :else     (recur next (chain-next next) (cons next chain)))))

(defn square-digit-chains [& [maxn]]
  (let [maxn (or maxn 10000000)]
    (loop [n     1
           set1  #{1}
           set89 #{89}]
      (if (= n maxn)
        (count set89)
        (let [chain (get-chain n set1 set89)
              place (first chain)
              new   (rest chain)]
          (cond
           (= place 1)  (recur (inc n) (sets/union set1 (set new)) set89)
           (= place 89) (recur (inc n) set1 (sets/union set89 (set new)))))))))
