(ns projecteuler.p087
  (:require [projecteuler.core :refer [primes]]))

;; https://projecteuler.net/problem=87

;; Brute-force approach adapted from
;; http://www.mathblog.dk/project-euler-87-sum-power/

(defn prime-power-triples [& [maxn]]
  (let [maxn      (or maxn 50000000)
        maxp      (int (Math/sqrt maxn))
        primelist (take-while #(< % maxp) primes)
        pl2       (map #(* % %) primelist)
        pl3       (map #(* % % %) primelist)
        pl4       (map #(* % %) pl2)]
    (loop [[square & list1] pl2, nums #{}]
      (cond
       (nil? square) (count nums)
       :else
       (recur list1
              (loop [[cube & list2] pl3, nums nums]
                (cond
                 (nil? cube)              nums
                 (> (+ square cube) maxn) nums
                 :else
                 (recur list2
                        (loop [[quad & list3] pl4, nums nums]
                          (let [sum (+ square cube quad)]
                              (cond
                               (nil? quad)  nums
                               (> sum maxn) nums
                               :else
                               (recur list3 (conj nums sum)))))))))))))
