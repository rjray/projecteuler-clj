(ns projecteuler.core)

; Resources (functions, lazy-seqs, etc.) shared by two or more of the Euler
; solutions.

(def fib-seq
  ((fn rfib [a b]
     (lazy-seq (cons a (rfib b (+' a b)))))
   1 1))

(defn is-palindrome-num? [x]
  (let [x-str (str x)
        x-seq (seq x-str)]
    (= x-seq (reverse x-seq))))

(defn prime? [n & [certainty]]
  (let [certainty (or certainty 5)]
    (.isProbablePrime (BigInteger/valueOf n) certainty)))

(def num-map {\0 0 \1 1 \2 2 \3 3 \4 4 \5 5 \6 6 \7 7 \8 8 \9 9})
(def uc-map
  (apply hash-map (flatten (map #(list (char (+ % 64)) %) (range 1 27)))))

(def primes
  (concat
   [2 3 5 7]
   (lazy-seq
    (let [primes-from
          (fn primes-from [n [f & r]]
            (if (some #(zero? (rem n %))
                      (take-while #(<= (* % %) n) primes))
              (recur (+ n f) r)
              (lazy-seq (cons n (primes-from (+ n f) r)))))
          wheel (cycle [2 4 2 4 6 2 6 4 2 4 6 6 2 6  4  2
                        6 4 6 8 4 2 4 2 4 8 6 4 6 2  4  6
                        2 6 6 4 2 4 6 2 6 4 2 4 2 10 2 10])]
      (primes-from 11 wheel)))))

(defn triangular-numbers []
  ((fn triangular [n acc]
     (let [val (+ n acc)]
       (cons val (lazy-seq (triangular (inc n) val))))) 1 0))

(defn ! [n] (reduce *' (range 1 (inc n))))

(defn word-score [w]
  (apply + (map uc-map w)))

(defn is-prime? [n]
  (= n (first (drop-while #(< % n) primes))))
