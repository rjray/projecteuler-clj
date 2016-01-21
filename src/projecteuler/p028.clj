(ns projecteuler.core)

;; https://projecteuler.net/problem=28

(def corners-seq
  ((fn next-corners [cur pos]
     (let [base   (last cur)
           factor (* 2 pos)
           new    (map #(+ base (* factor %)) (range 1 5))]
       (lazy-seq (cons cur (next-corners new (inc pos))))))
   '(1) 1))
(defn number-spiral-diagonals [& [size]]
  (let [s (or size 1001)
        c (/ (inc s) 2)]
    (if (even? s)
      nil
      (apply + (flatten (take c corners-seq))))))
