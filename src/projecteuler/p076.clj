(ns projecteuler.core)

;; https://projecteuler.net/problem=76

;; Attempt: Using dynamic programming

(defn- calc-row [y nums matrix]
  (loop [x 1
         m matrix]
    (cond
     (= x (count nums))
         m
     (>= y (nums x))
         (recur (inc x)
                (assoc-in m [x y] (+ (get-in m [(dec x) y])
                                     (get-in m [x (- y (nums x))]))))
     :else
         (recur (inc x)
                (assoc-in m [x y] (get-in m [(dec x) y]))))))

(defn count-summations [& [target]]
  (let [target (or target 100)
        nums   (vec (range 1 target))
        matrix (vec (cons (vec (repeat (inc target) 1))
                          (vec (repeat (dec (count nums))
                                       (vec (repeat (inc target) 0))))))]
    (loop [sum    0
           matrix matrix]
      (cond
       (> sum target) (last (last matrix))
       :else (recur (inc sum) (calc-row sum nums matrix))))))
