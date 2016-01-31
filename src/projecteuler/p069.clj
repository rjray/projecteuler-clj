(ns projecteuler.core
  (:require [projecteuler.core :refer [factorize]]))

;; https://projecteuler.net/problem=69

; See:
;   https://stackoverflow.com/questions/1019040/how-many-numbers-below-n-are-coprimes-to-n
;   https://en.wikipedia.org/wiki/Euler's_totient_function

(defn- factorize-pairs [n]
  (map #(list (first %) (count (second %))) (group-by identity (factorize n))))
(defn- totient [n]
  (reduce * 1 (for [[p e] (factorize-pairs n)]
                (* (dec p) (math/expt p (dec e))))))

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
