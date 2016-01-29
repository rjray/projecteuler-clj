(ns projecteuler.core)

;; https://projecteuler.net/problem=39

(defn- euclid-solutions [m n max]
  (let [a (- (* m m) (* n n))
        b (* 2 m n)
        c (+ (* m m) (* n n))
        p (+ a b c)
        q (quot max p)]
    (map #(vec (sort (list (* a %) (* b %) (* c %) (* p %))))
         (range 1 (inc q)))))
(defn- get-euclid-solutions [max-p]
  (let [max-n (int (Math/sqrt (/ max-p 2)))
        max-m (inc max-n)]
    (loop [m 1, c #{}]
      (cond
       (> m max-m) c
       :else (recur (inc m)
                    (set (reduce concat c
                                 (map #(euclid-solutions % m max-p)
                                      (range (inc m) max-n 2)))))))))

(defn int-right-triangles-max [& [max]]
  (let [max (or max 999)]
    (first (first (sort #(compare (count (second %2))
                                  (count (second %1)))
                        (group-by last (get-euclid-solutions max)))))))
