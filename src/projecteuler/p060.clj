(ns projecteuler.p060
  (:require [projecteuler.core :refer [primes is-prime?]]))

;; https://projecteuler.net/problem=60

(defn- is-prime-pair? [a b]
  (and (is-prime? (Long/parseUnsignedLong (str a b)))
       (is-prime? (Long/parseUnsignedLong (str b a)))))

(defn- pairable-primes [a ps]
  (filter #(is-prime-pair? a %) ps))

(defn- brute-force [plist n remaining-primes & [curmin]]
  (let [curmin (or curmin Long/MAX_VALUE)]
    (cond
     (= n (count plist))          (do
                                    (prn (sort plist) '= (apply + plist))
                                    (min curmin (apply + plist)))
     (not (seq remaining-primes)) curmin
     :else
     (loop [[p & ps] remaining-primes
            curmin curmin]
       (cond
        (nil? p) curmin
        :else    (recur ps
                        (brute-force (cons p plist)
                                     n
                                     (pairable-primes p ps)
                                     curmin)))))))

(defn prime-pair-sets-bruteforce [& [n maxp]]
  (let [n      (or n 5)
        maxp   (or maxp 10000)
        pool   (rest (take-while #(< % maxp) primes))]
    (brute-force '() n pool)))
