(ns projecteuler.p045)

;; https://projecteuler.net/problem=45

(defn- T [n] (/ (* n (inc n)) 2))
(defn- P [n] (/ (* n (dec (* 3 n))) 2))
(defn- H [n] (* n (dec (* 2 n))))

;; Avg elapsed time: 66.650ms (Linux/rjray)
(defn tri-pen-hex [& [init-t init-p init-h]]
  (let [init-t (or init-t 286)
        init-p (or init-p 166)
        init-h (or init-h 144)]
    (loop [t init-t, p init-p, h init-h]
      (let [tval (T t), pval (P p), hval (H h)]
        (cond
          (= tval pval hval) (list tval t p h)
          (= tval pval)      (if (< hval tval)
                               (recur t p (inc h))
                               (recur (inc t) (inc p) h))
          :else              (if (< tval pval)
                               (recur (inc t) p h)
                               (recur t (inc p) h)))))))

;; Avg elapsed time: 23.887ms (Linux/rjray)
(defn tri-pen-hex-2 [& [init-p init-h]]
  (let [init-p (or init-p 166)
        init-h (or init-h 144)]
    (loop [p init-p, h init-h]
      (let [pval (P p), hval (H h)]
        (cond
          (= pval hval) (list pval p h)
          :else         (if (< hval pval)
                          (recur p (inc h))
                          (recur (inc p) h)))))))
