(ns projecteuler.core)

;; https://projecteuler.net/problem=6

(defn sum-square-diff [& [num]]
  (let [num     (or num 100)
        sum     (/ (* num (inc num)) 2)
        sqr-sum (* sum sum)
        sum-sqr (apply + (map #(* % %) (range 1 (inc num))))]
    (- sqr-sum sum-sqr)))
