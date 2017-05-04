(ns projecteuler.p142)

;; https://projecteuler.net/problem=142

;; Based on the brute-force C# code at:
;; http://www.mathblog.dk/project-euler-142-perfect-square-collection/

(defn- is-square [n]
  (let [rootn (int (Math/sqrt n))]
    (= n (* rootn rootn))))

(defn- k-loop [j a c f]
  (loop [k (if (odd? j) 1 2)]
    (cond
     (>= k j) nil
     :else
     (let [d (* k k), e (- a d), b (- c e)]
       (cond
        (<= b 0)            (recur (+ k 2))
        (<= e 0)            (recur (+ k 2))
        (not (is-square b)) (recur (+ k 2))
        (not (is-square e)) (recur (+ k 2))
        :else
        (let [x (/ (+ d c) 2), y (/ (+ e f) 2), z (/ (- c d) 2)]
          (list x y z (+ x y z))))))))

(defn- j-loop [i a]
  (loop [j 3]
    (let [c (* j j), f (- a c)]
      (cond
       (= j i)             nil
       (<= f 0)            (recur (inc j))
       (not (is-square f)) (recur (inc j))
       :else
       (let [soln (k-loop j a c f)]
         (cond
          (seq? soln) soln
          :else       (recur (inc j))))))))

(defn perfect-square-collection []
  (loop [i 4]
    (let [soln (j-loop i (* i i))]
      (cond
       (seq? soln) soln
       :else       (recur (inc i))))))
