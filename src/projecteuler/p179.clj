(ns projecteuler.p179)

;; https://projecteuler.net/problem=179

;; Based on the dynamic-ish algorithm at
;; https://blog.dreamshire.com/project-euler-179-solution/

(defn- apply-factor [i n v]
  (loop [j (* i 2), v v]
    (cond
     (>= j n) v
     :else   (recur (+ j i) (update v j inc)))))

;; Slow. Try to parallel-ize this function.
(defn- dynamic-divisors [n]
  (let [v (vec (repeat n 0))
        m (int (/ n 2))]
    (loop [i 2, v v]
      (cond
       (> i m) v
       :else   (recur (inc i) (apply-factor i n v))))))

(defn consecutive-positive-divisors [& [maxn]]
  (let [n  (or maxn 10000000)
        dv (dynamic-divisors n)]
    (apply + (for [i (range 3 n)]
               (cond
                (= (dv i) (dv (dec i))) 1
                :else                   0)))))
