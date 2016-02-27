(ns projecteuler.p015)

;; https://projecteuler.net/problem=15

;; See also https://en.wikipedia.org/wiki/Binomial_coefficient
(defn lattice-paths [& [k]]
  (let [k (or k 20)
        n (* k 2)]
    (apply * (map #(/ (- (+ n 1) %) %) (range 1 (inc k))))))
