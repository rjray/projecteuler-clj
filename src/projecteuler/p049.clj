(ns projecteuler.p049
  (:require [projecteuler.core :refer [primes]]))

;; https://projecteuler.net/problem=49

(defn- are-permutations? [n o p]
  (= (sort (seq (str n)))
     (sort (seq (str o)))
     (sort (seq (str p)))))

(defn- find-permutations [plist]
  (let [[n & ps] plist, pset (set ps)]
    (loop [ps ps]
      (cond
       (empty? ps) nil
       :else
       (let [o    (first ps)
             diff (- o n)
             p    (+ o diff)]
         (cond
          (and (pset p) (are-permutations? n o p))
              (list n o p diff)
          :else
              (recur (rest ps))))))))

(defn prime-permutations []
  (let [primeset (->> primes
                      (drop-while #(< % 1000))
                      (take-while #(< % 10000)))]
    (loop [primeset primeset
           soln     ()]
      (cond
       (empty? primeset)
           (filter seq soln)
       :else
           (recur (rest primeset) (cons (find-permutations primeset) soln))))))
