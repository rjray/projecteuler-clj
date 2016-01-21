(ns projecteuler.core)

;; https://projecteuler.net/problem=14

(defn- collatz-next [n]
  (if (even? n) (/ n 2) (inc (* n 3))))
(defn- collatz-chain-recursive [n]
  (if (= n 1) 1
      (inc (collatz-chain-recursive (collatz-next n)))))
(defn longest-collatz-sequence [& [maximum]]
  (let [maximum (or maximum 1000000)]
    (first (apply max-key second
                  (map #(list % (collatz-chain-recursive %))
                       (range 1 maximum))))))
