(ns projecteuler.p026)

;; https://projecteuler.net/problem=26

(defn- find-cycle [e l p]
  (cond
   (empty? l) -1
   (= e (first l)) p
   :else
   (recur e (rest l) (inc p))))

(defn- find-cycle-length [numer n l]
  (loop [numer numer
         n     n
         l     l]
    (let [d (int (Math/floor (/ numer n)))]
      (cond
       (some #(= % [d numer]) l)
           [(inc (find-cycle [d numer] l 0)) n]
       (zero? numer)
           [0 n]
       (< numer n)
           (recur (* 10 numer) n (cons [0 n] l))
       :else
           (recur (* 10 (rem numer n)) n (cons [d numer] l))))))

(defn reciprocal-cycles [& [d]]
  (let [d (or d 1000)]
    (second (first (sort #(compare (first %2) (first %1))
                         (map #(find-cycle-length 10 % []) (range 1 d)))))))
