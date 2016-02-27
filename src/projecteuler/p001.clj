(ns projecteuler.p001)

;; https://projecteuler.net/problem=1

(defn sum-mult-3-or-5 [& [count]]
  (reduce + (filter #(or (zero? (mod % 3))
                         (zero? (mod % 5)))
                    (range 1 (or count 1000)))))
