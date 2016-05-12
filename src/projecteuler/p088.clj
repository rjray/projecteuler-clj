(ns projecteuler.p088)

;; https://projecteuler.net/problem=88

;; Based on the Java code at
;; http://www.cnblogs.com/theskulls/p/5002988.html

(defn- check [prod sum k]
  (cond
   (< sum k)  false
   (= prod 1) (= sum k)
   (= k 1)    (= prod sum)
   :else      (loop [d 2]
                (cond
                 (or (> d prod)
                     (< (- sum d) (dec k)))
                     false
                 (and (zero? (mod prod d))
                      (check (/ prod d) (- sum d) (dec k)))
                     true
                 :else
                     (recur (inc d))))))

(defn- get-min [k]
  (loop [n (inc k)]
    (cond
     (check n n k) n
     :else         (recur (inc n)))))

;; Roughly 19.566s
(defn product-sum-numbers [& [kmax]]
  (let [kmax (or kmax 12000)]
    (reduce + (set (pmap get-min (range 2 (inc kmax)))))))
