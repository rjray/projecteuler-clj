(ns projecteuler.core)

;; https://projecteuler.net/problem=29

(defn distinct-powers [& [n]]
  (let [n (or n 100)]
    (count (set (for [a (range 2 (inc n)) b (range 2 (inc n))]
                  (apply *' (repeat b a)))))))
