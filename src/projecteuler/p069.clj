(ns projecteuler.p069
  (:require [projecteuler.core :refer [totient]]))

;; https://projecteuler.net/problem=69

(defn totient-maximum-loop [& [max]]
  (let [max (or max 1000000)]
    (loop [n 2, maxn 0, maxnphi 0]
      (cond
       (> n max) maxn
       :else
       (let [phi    (totient n)
             nphi   (/ n phi)
             nmax   (if (> nphi maxnphi) n maxn)
             phimax (if (> nphi maxnphi) nphi maxnphi)]
         (recur (inc n) nmax phimax))))))

;; 988238.434 msecs
(defn totient-maximum [& [max]]
  (let [max (or max 1000000)]
    (first (first (sort #(compare (last %2) (last %1))
                        (pmap #(list % (/ % (totient %)))
                              (range 2 (inc max))))))))
