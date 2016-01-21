(ns projecteuler.core)

;; https://projecteuler.net/problem=52

(defn permuted-multiples []
  (let [stream  (iterate inc 1)
        mults   (fn [x] (map #(* x %) (range 1 7)))
        to-set  (fn [x] (set (str x)))
        matches (fn [x] (apply = (map to-set (mults x))))]
    (first (filter matches stream))))
