(ns projecteuler.p357)

;; https://projecteuler.net/problem=357

;; Based on the algorithm found here:
;; https://www.coastalvectors.com/blog/2015/06/project-euler-357/

;; This prime generator seems to be the fastest at getting the primes up to
;; 10^8. It comes from:
;; https://codereview.stackexchange.com/questions/28902/clojure-code-for-finding-prime-numbers

(defn- primes-up-to [n]
  (let [mark (fn [i di v]
               (if (<= i (count v))
                 (recur (+ i di) di (assoc v (dec i) di))
                 v))
        step (fn [i ps v]
               (if (<= i (count v))
                 (if (= (v (dec i)) 1)
                   (recur (inc i) (conj ps i) (mark (* i i) i v))
                   (recur (inc i) ps v))
                 ps))]
    (->> (repeat n 1) vec (step 2 []))))

(defn- is-prime-gen [n primes]
  (let [sqrtn (int (Math/sqrt n))]
    (loop [d 2]
      (cond
       (>= d sqrtn)                        true
       (and (zero? (mod n d))
            (nil? (primes (+ d (/ n d))))) false
       :else
       (recur (inc d))))))

(defn prime-generating-integers [& [limit]]
  (let [limit  (or limit 100000000)
        primes (set (primes-up-to limit))]
    (loop [n 0, sum 1]
      (let [candidate (+ (* 4 n) 2)]
        (cond
          (>= candidate limit)                  sum
          (and (primes (inc candidate))
               (primes (+ 2 (/ candidate 2)))
               (is-prime-gen candidate primes)) (recur (inc n) (+ sum candidate))
          :else                                 (recur (inc n) sum))))))
