(ns projecteuler.p078)

;; https://projecteuler.net/problem=78

;; Pentagon number generator:
(defn- P [n] (/ (* n (dec (* 3 n))) 2))

;; Algorithm from http://blog.dreamshire.com/project-euler-78-solution/
(defn coin-partitions []
  (let [k   (vec (flatten (map #(list (P %) (+ (P %) %)) (range 1 250))))
        sgn [1 1 -1 -1]]
    (loop [p [1], n 0]
      (cond
        (zero? (p n)) n
        :else  (let [n  (inc n)
                     px (loop [px 0, i 0]
                          (cond
                            (> (k i) n) px
                            :else
                            (recur (+ px (* (p (- n (k i))) (sgn (mod i 4))))
                                   (inc i))))]
                 (recur (conj p (mod px 1000000)) n))))))
