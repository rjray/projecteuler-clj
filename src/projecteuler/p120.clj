(ns projecteuler.p120)

;; https://projecteuler.net/problem=120
;; See analysis at https://www.mathblog.dk/project-euler-120-maximum-remainder/

(defn sum-max-remainder [from to]
  (reduce + (map #(* 2 % (int (/ (dec %) 2))) (range from (inc to)))))
