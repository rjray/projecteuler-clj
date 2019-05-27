(ns projecteuler.p125
  (:require [projecteuler.core :refer [is-palindrome-num?]]))

;; https://projecteuler.net/problem=125

;; Brute-force-ish approach

(defn- all-palin-sums [l maxval]
  (loop [l l, sums ()]
    (cond
      (= 1 (count l)) (filter is-palindrome-num? (filter #(< % maxval) sums))
      :else           (recur (rest l) (cons (apply + l) sums)))))

(defn- collect-sums [maxval]
  (loop [sums (), squares (), x 1]
    (let [sqx     (* x x)
          squares (concat squares (list sqx))]
      (cond
        (>= sqx maxval) (sort (distinct sums))
        :else
        (recur (concat sums (all-palin-sums squares maxval))
               squares
               (inc x))))))

;; Takes too long:
(defn palindromic-sums-brute [& [maxval]]
  (apply + (collect-sums (or maxval 100000000))))

;; Based on the brute-force two-loop approach here:
;; https://www.mathblog.dk/project-euler-125-square-sums-palindromic/
(defn palindromic-sums [& [maxval]]
  (let [maxval   (or maxval 100000000)
        sqrt-max (int (Math/sqrt maxval))]
    (loop [i 1, sums #{}]
      (let [number (* i i)]
        (cond
          (> i sqrt-max) (apply + sums)
          :else
          (recur (inc i)
                 (loop [j (inc i), number number, sums sums]
                   (let [number (+ number (* j j))]
                     (cond
                       (>= j sqrt-max)    sums
                       (>= number maxval) sums
                       (is-palindrome-num? number)
                       (recur (inc j)
                              number
                              (conj sums number))
                       :else
                       (recur (inc j) number sums))))))))))
