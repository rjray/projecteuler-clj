(ns projecteuler.p070
  (:require [projecteuler.core :refer [totient primes]]))

;; https://projecteuler.net/problem=70

(defn- tot-is-permutation? [n tn]
  (let [ns  (sort (seq (str n)))
        tns (sort (seq (str tn)))]
    (= ns tns)))

(defn- fast-totient [p1 p2] (* (dec p1) (dec p2)))

(defn- get-permutation-totients [maxn prime-list]
  (filter seq?
          (for [p1 prime-list, p2 (filter #(> % p1) prime-list)]
            (let [n (* p1 p2)
                  t (fast-totient p1 p2)]
              (if (and (< n maxn) (tot-is-permutation? n t))
                (list n (/ n t)))))))

(defn totient-permutation [& [maxn]]
  (let [maxn      (or maxn 10000000)
        maxpr     (int (/ maxn 1000))
        pr-list   (filter #(> % 1000) (take-while #(< % maxpr) primes))
        perm-list (get-permutation-totients maxn pr-list)]
    (->> perm-list
         (sort #(compare (second %1) (second %2)))
         (first))))

;; Took 17.254 hours on Linux/rjray
;; (Requires arity of tot-is-permutation to be changed)
(defn totient-permutation-brute [& [n]]
  (->> (or n 10000000)
       (range 2)
       (pmap #(list % (totient %)))
       (filter tot-is-permutation?)
       (map #(list (first %) (apply / %)))
       (sort #(compare (second %1) (second %2)))
       (first)))
