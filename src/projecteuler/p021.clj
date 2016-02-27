(ns projecteuler.p021)

;; https://projecteuler.net/problem=21

(defn- sum-proper-divisors [n]
  (cond
   (< n 2) (- n)
   true (let [base (filter #(zero? (mod n %)) (range 2 (+ (Math/sqrt n) 1)))]
          (reduce + 1 (set (concat (map #(/ n %) base) base))))))
(defn- amicable? [n sums]
  (let [sum (sums n)
        m   (get sums sum)]
    (and (not (= n sum))
         (= n m))))
(defn sum-amicable-numbers [& [n]]
  (let [n (or n 10000)
        nums (vec (map sum-proper-divisors (range n)))]
    (apply + (filter #(amicable? % nums) (range 2 n)))))
