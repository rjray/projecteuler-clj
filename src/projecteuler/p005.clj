(ns projecteuler.p005)

;; https://projecteuler.net/problem=5

(defn smallest-multiple []
  (let [rems (fn [x] (apply + (map #(rem x %) (range 2 21))))]
    (loop [n (* 19 20)]
      (cond
       (zero? (rems n)) n
       (< (- Long/MAX_VALUE n) 20) nil
       true (recur (+ n 20))))))

(defn smallest-multiple-2 []
  (let [gcd (fn [a b] (if (zero? b) a (recur b (mod a b))))
        lcm (fn [a b] (/ (* a b) (gcd a b)))]
    (reduce #(lcm %1 %2) (range 1 21))))
