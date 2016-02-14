(ns projecteuler.core)

;; https://projecteuler.net/problem=71

(defn ordered-fractions [& [d]]
  (let [d (or d 1000000)]
    (loop [n   2
           cur 0]
      (let [r (/ (int (* 3/7 n)) n)]
        (cond
         (> n d)   cur
         (= r 3/7) (recur (inc n) cur)
         (> r cur) (recur (inc n) r)
         :else     (recur (inc n) cur))))))
