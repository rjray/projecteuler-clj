(ns projecteuler.p003)

;; https://projecteuler.net/problem=3

;; (defn- get-largest-prime-factor [num cur limit]
;;   (if (> cur limit)
;;   num
;;   (if (= num cur)
;;     num
;;     (if (zero? (mod num cur))
;;       (get-largest-prime-factor (/ num cur) cur limit)
;;       (get-largest-prime-factor num (inc cur) limit)))))
;; (defn largest-prime-factor [& [num]]
;;   (let [num   (or num 600851475143)
;;         limit (long (Math/sqrt num))]
;;     (get-largest-prime-factor num 2 limit)))
(defn largest-prime-factor [& [num]]
  (let [num     (or num 600851475143)
        q       (long (Math/sqrt num))
        factor? (fn [a b] (zero? (rem a b)))]
    (loop [n num d 2]
      (cond
       (> d q)       n
       (= d n)       n
       (factor? n d) (recur (/ n d) d)
       true          (recur n (inc d))))))
