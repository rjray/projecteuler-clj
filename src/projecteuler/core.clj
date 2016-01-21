(ns projecteuler.core
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as comb]))

;; https://projecteuler.net/problem=1
(defn sum-mult-3-or-5 [& [count]]
  (reduce + (filter #(or (zero? (mod % 3))
                         (zero? (mod % 5)))
                    (range 1 (or count 1000)))))

;; https://projecteuler.net/problem=2
(def fib-seq
  ((fn rfib [a b]
     (lazy-seq (cons a (rfib b (+' a b)))))
   1 1))
(defn sum-even-fibo-terms [& [maximum]]
  (let [maximum (or maximum 4000000)]
    (reduce + (filter even? (take-while #(< % maximum) fib-seq)))))

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

;; https://projecteuler.net/problem=4
(defn- is-palindrome-num? [x]
  (let [x-str (str x)
        x-seq (seq x-str)]
    (= x-seq (reverse x-seq))))
(defn largest-palindrome-product []
  (apply max
         (filter is-palindrome-num?
                 (for [a (range 101 1000)
                       b (range 101 1000)]
                   (* a b)))))

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

;; https://projecteuler.net/problem=6
(defn sum-square-diff [& [num]]
  (let [num     (or num 100)
        sum     (/ (* num (inc num)) 2)
        sqr-sum (* sum sum)
        sum-sqr (apply + (map #(* % %) (range 1 (inc num))))]
    (- sqr-sum sum-sqr)))

;; https://projecteuler.net/problem=7
(defn- prime? [n & [certainty]]
  (let [certainty (or certainty 5)]
    (.isProbablePrime (BigInteger/valueOf n) certainty)))
(defn nth-prime [& [n]]
  (let [n (or n 10001)]
    (last
     (take (dec n)
           (filter prime? (take-nth 2 (range 1 Integer/MAX_VALUE)))))))

;; https://projecteuler.net/problem=8
(def target-number
  (str "73167176531330624919225119674426574742355349194934"
       "96983520312774506326239578318016984801869478851843"
       "85861560789112949495459501737958331952853208805511"
       "12540698747158523863050715693290963295227443043557"
       "66896648950445244523161731856403098711121722383113"
       "62229893423380308135336276614282806444486645238749"
       "30358907296290491560440772390713810515859307960866"
       "70172427121883998797908792274921901699720888093776"
       "65727333001053367881220235421809751254540594752243"
       "52584907711670556013604839586446706324415722155397"
       "53697817977846174064955149290862569321978468622482"
       "83972241375657056057490261407972968652414535100474"
       "82166370484403199890008895243450658541227588666881"
       "16427171479924442928230863465674813919123162824586"
       "17866458359124566529476545682848912883142607690042"
       "24219022671055626321111109370544217506941658960408"
       "07198403850962455444362981230987879927244284909188"
       "84580156166097919133875499200524063689912560717606"
       "05886116467109405077541002256983155200055935729725"
       "71636269561882670428252483600823257530420752963450"))
(def num-map {\0 0 \1 1 \2 2 \3 3 \4 4 \5 5 \6 6 \7 7 \8 8 \9 9})
(defn- max-sequence [sequence n]
  (loop [s   sequence
         n   n
         acc 0]
    (let [n-seq (take n s)
          value (reduce * (map num-map n-seq))
          acc   (max acc value)]
      (cond
       (<= n (count n-seq)) (recur (rest s) n acc)
       true                   acc))))
(defn largest-series-product [& [digits n]]
  (let [digits (or digits target-number)
        n      (or n 13)]
    (apply max (map #(max-sequence % n) (str/split digits #"0+")))))

;; https://projecteuler.net/problem=9
(defn special-pythagorean-triplet [& [target]]
  (let [target     (or target 1000)
        is-target? (fn [a b c] (= target (+ a b c)))
        too-high?  (fn [a b c] (< target (+ a b c)))]
    (loop [a 3
           b 4]
      (let [cfloat (Math/sqrt (+ (* a a) (* b b)))
            c      (int cfloat)]
        (if (and (== c cfloat)
                 (is-target? a b c))
          (* a b c)
          (if (= b 998)
            (recur (+ a 1) (+ a 2))
            (recur a (inc b))))))))

;; https://projecteuler.net/problem=10
(defn summation-of-primes [& [upper]]
  (let [upper (or upper 2000000)]
    (apply + (filter #(prime? % 10) (range 2 upper)))))

;; https://projecteuler.net/problem=11
(def num-grid
  '(( 8  2 22 97 38 15  0 40  0 75  4  5  7 78 52 12 50 77 91  8)
    (49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48  4 56 62  0)
    (81 49 31 73 55 79 14 29 93 71 40 67 53 88 30  3 49 13 36 65)
    (52 70 95 23  4 60 11 42 69 24 68 56  1 32 56 71 37  2 36 91)
    (22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80)
    (24 47 32 60 99  3 45  2 44 75 33 53 78 36 84 20 35 17 12 50)
    (32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70)
    (67 26 20 68  2 62 12 20 95 63 94 39 63  8 40 91 66 49 94 21)
    (24 55 58  5 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72)
    (21 36 23  9 75  0 76 44 20 45 35 14  0 61 33 97 34 31 33 95)
    (78 17 53 28 22 75 31 67 15 94  3 80  4 62 16 14  9 53 56 92)
    (16 39  5 42 96 35 31 47 55 58 88 24  0 17 54 24 36 29 85 57)
    (86 56  0 48 35 71 89  7  5 44 44 37 44 60 21 58 51 54 17 58)
    (19 80 81 68  5 94 47 69 28 73 92 13 86 52 17 77  4 89 55 40)
    ( 4 52  8 83 97 35 99 16  7 97 57 32 16 26 26 79 33 27 98 66)
    (88 36 68 87 57 62 20 72  3 46 33 67 46 55 12 32 63 93 53 69)
    ( 4 42 16 73 38 25 39 11 24 94 72 18  8 46 29 32 40 62 76 36)
    (20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74  4 36 16)
    (20 73 35 29 78 31 90  1 74 31 49 71 48 86 81 16 23 57  5 54)
    ( 1 70 54 71 83 51 54 69 16 92 33 48 61 43 52  1 89 19 67 48)))
(defn- num-at [x y grid]
  (nth (nth grid y '()) x 0))
(defn- prod-right [x y grid]
  (* (num-at x       y grid)
     (num-at (+ x 1) y grid)
     (num-at (+ x 2) y grid)
     (num-at (+ x 3) y grid)))
(defn- prod-down [x y grid]
  (* (num-at x y       grid)
     (num-at x (+ y 1) grid)
     (num-at x (+ y 2) grid)
     (num-at x (+ y 3) grid)))
(defn- prod-diag-d [x y grid]
  (* (num-at x       y       grid)
     (num-at (+ x 1) (+ y 1) grid)
     (num-at (+ x 2) (+ y 2) grid)
     (num-at (+ x 3) (+ y 3) grid)))
(defn- prod-diag-u [x y grid]
  (* (num-at x       y       grid)
     (num-at (+ x 1) (- y 1) grid)
     (num-at (+ x 2) (- y 2) grid)
     (num-at (+ x 3) (- y 3) grid)))
(defn product-in-a-grid [& [grid]]
  (let [grid  (or grid num-grid)
        y-max (count grid)
        x-max (count (first grid))]
    (loop [x   0
           y   0
           acc 0]
      (cond
       (= y y-max) acc
       (= x x-max) (recur 0 (inc y) acc)
       true        (recur (inc x) y (max acc
                                         (prod-right  x y grid)
                                         (prod-down   x y grid)
                                         (prod-diag-u x y grid)
                                         (prod-diag-d x y grid)))))))

;; https://projecteuler.net/problem=12
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
(defn- triangular-numbers []
  ((fn triangular [n acc]
     (let [val (+ n acc)]
       (cons val (lazy-seq (triangular (inc n) val))))) 1 0))
(defn- factorize [n]
  (loop [x n [p & ps] primes factors []]
    (cond (= 1 x) factors
          (zero? (mod x p)) (recur (/ x p) primes (conj factors p))
          :else (recur x ps factors))))
(defn- factorize-count [n]
  (reduce * (map (comp inc count) (vals (group-by identity (factorize n))))))
(defn highly-divisible-triangular-number [& [n]]
  (let [n (or n 500)]
    (first (drop-while #(< (factorize-count %) n) (triangular-numbers)))))

;; https://projecteuler.net/problem=13
(defn large-sum [& [source]]
  (let [source  (or source "data/013.txt")
        numbers (slurp source)]
    (read-string
     (apply str
            (take 10
                  (str (apply + (map bigint (re-seq #"\d+" numbers)))))))))

;; https://projecteuler.net/problem=14
(defn- collatz-next [n]
  (if (even? n) (/ n 2) (inc (* n 3))))
(defn- collatz-chain-recursive [n]
  (if (= n 1) 1
      (inc (collatz-chain-recursive (collatz-next n)))))
(defn longest-collatz-sequence [& [maximum]]
  (let [maximum (or maximum 1000000)]
    (first (apply max-key second
                  (map #(list % (collatz-chain-recursive %))
                       (range 1 maximum))))))

;; https://projecteuler.net/problem=15
;; See also https://en.wikipedia.org/wiki/Binomial_coefficient
(defn lattice-paths [& [k]]
  (let [k (or k 20)
        n (* k 2)]
    (apply * (map #(/ (- (+ n 1) %) %) (range 1 (inc k))))))

;; https://projecteuler.net/problem=16
(defn power-digit-sum [& [x n]]
  (let [x (or x 2)
        n (or n 1000)]
   (reduce + (map num-map (.toString (.pow (BigInteger. (str x)) n))))))

;; https://projecteuler.net/problem=17

;; https://projecteuler.net/problem=18
(defn- triangle [file]
  (map #(Integer/parseInt %) (re-seq #"\d\d" (slurp file))))
(defn- triangle-tree [s]
  (loop [n    1
         s    s
         tree nil]
    (cond
     (empty? s) tree
     true (recur (inc n) (drop n s) (cons (take n s) tree)))))
(defn- reduce-row [s]
  (map #(reduce max %) (partition 2 1 s)))
(defn- combine-rows [s1 s2]
  (map + (reduce-row s1) s2))
(defn maximum-path-sum [& [file]]
  (let [file (or file "data/018.txt")
        data (triangle file)
        tree (triangle-tree data)]
    (first (reduce combine-rows tree))))

;; https://projecteuler.net/problem=19

;; https://projecteuler.net/problem=20
(defn- ! [n] (reduce *' (range 1 (inc n))))
(defn factorial-digit-sum [& [n]]
  (let [n (or n 100)
        v (! n)
        s (str v)]
    (reduce + (map num-map s))))

;; https://projecteuler.net/problem=21
(defn- sum-proper-divisors [n]
  (cond
   (< n 2) (- n)
   true (let [base (filter #(zero? (mod n %)) (range 2 (+ (Math/sqrt n) 1)))]
          (reduce + 1 (set (concat (map #(/ n %) base) base))))))
(defn- amicable? [n sums]
  (let [sum (sums n)
        m   (get sums sum)]
    (and (not (= n sum))
         (= n m))))
(defn sum-amicable-numbers [& [n]]
  (let [n (or n 10000)
        nums (vec (map sum-proper-divisors (range n)))]
    (apply + (filter #(amicable? % nums) (range 2 n)))))

;; https://projecteuler.net/problem=22
(def uc-map
  (apply hash-map (flatten (map #(list (char (+ % 64)) %) (range 1 27)))))
(defn- word-score [w]
  (apply + (map uc-map w)))
(defn names-scores [& [file]]
  (let [file  (or file "data/022.txt")
        data  (slurp file)
        names (sort (re-seq #"[A-Z]+" data))]
    (reduce + (map-indexed #(* (inc %1) (word-score %2)) names))))

;; https://projecteuler.net/problem=23

;; https://projecteuler.net/problem=24
(defn lexicographic-permutations [& [n]]
  (let [n (or n 1000000)]
    (->> (range 10)
         (comb/permutations)
         (drop (dec n))
         (first)
         (apply str))))

;; https://projecteuler.net/problem=25
(defn fibo-1000 []
  (first (first (drop-while #(< (second %) 1000)
                            (map-indexed #(list (inc %1)
                                                (count (str %2))) fib-seq)))))

;; https://projecteuler.net/problem=26

;; https://projecteuler.net/problem=27
(defn- is-prime? [n]
  (= n (first (drop-while #(< % n) primes))))
(defn- quadratic [n a b]
  (+ (* n n) (* a n) b))
(defn- prime-list [a b]
  (take-while is-prime? (map #(quadratic % a b) (iterate inc 0))))
(defn quadratic-primes-prod []
  (second (first (sort #(compare (first %2) (first %1))
                       (for [a (range -999 1000), b (range -999 1000)]
                         (list (count (prime-list a b)) (* a b)))))))

;; https://projecteuler.net/problem=28
(def corners-seq
  ((fn next-corners [cur pos]
     (let [base   (last cur)
           factor (* 2 pos)
           new    (map #(+ base (* factor %)) (range 1 5))]
       (lazy-seq (cons cur (next-corners new (inc pos))))))
   '(1) 1))
(defn number-spiral-diagonals [& [size]]
  (let [s (or size 1001)
        c (/ (inc s) 2)]
    (if (even? s)
      nil
      (apply + (flatten (take c corners-seq))))))

;; https://projecteuler.net/problem=29
(defn distinct-powers [& [n]]
  (let [n (or n 100)]
    (count (set (for [a (range 2 (inc n)) b (range 2 (inc n))]
                  (apply *' (repeat b a)))))))

;; https://projecteuler.net/problem=30
(defn digit-nth-powers-sum-naive [& [n]]
  (let [n   (or n 5)
        pow (fn [a] (apply * (repeat n a)))
        sum (fn [a] (apply + (map pow (map num-map (str a)))))
        max (* (inc n) (pow 9))]
    (reduce + (filter #(= % (sum %)) (range 10 (inc max))))))

;; https://projecteuler.net/problem=34
(defn digit-factorials-sum []
  (let [fact-map (apply hash-map
                        (flatten (map #(list (char (+ % 48)) (! %))
                                      (range 10))))
        sum      (fn [a] (apply + (map fact-map (str a))))]
    (apply + (filter #(= % (sum %)) (range 10 1000000)))))

;; https://projecteuler.net/problem=36
(defn- is-palindrome-binary-num? [x]
  (let [x-str (Integer/toString x 2)
        x-seq (seq x-str)]
    (= x-seq (reverse x-seq))))
(defn double-base-palindromes [& [maximum]]
  (let [maximum (or maximum 1000000)]
    (reduce + (filter is-palindrome-binary-num?
                      (filter is-palindrome-num? (range 1 maximum))))))

;; https://projecteuler.net/problem=42
(defn coded-triangle-numbers [& [file]]
  (let [file      (or file "data/042.txt")
        data      (slurp file)
        words     (re-seq #"\w+" data)
        maxlen    (apply max (map count words))
        maxcode   (* maxlen 26)
        triangles (set (take-while #(< % maxcode) (triangular-numbers)))]
    (apply + (map #(if (nil? (triangles %)) 0 1) (map word-score words)))))

;; https://projecteuler.net/problem=48
(defn- pow' [a]
  (reduce *' (repeat a a)))
(defn self-powers [& [max]]
  (let [max (or max 1000)]
    (apply str
           (reverse (take 10
                          (reverse (str (reduce +'
                                                (map pow'
                                                     (range 1
                                                            (inc max)))))))))))

;; https://projecteuler.net/problem=52
(defn permuted-multiples []
  (let [stream  (iterate inc 1)
        mults   (fn [x] (map #(* x %) (range 1 7)))
        to-set  (fn [x] (set (str x)))
        matches (fn [x] (apply = (map to-set (mults x))))]
    (first (filter matches stream))))

;; https://projecteuler.net/problem=67
(defn maximum-path-sum-2 []
  (maximum-path-sum "data/067.txt"))

;; https://projecteuler.net/problem=97
(defn large-non-mersenne-prime []
  (apply str
         (reverse (take 10
                        (reverse (str (+ (* 28433
                                            (.pow (BigInteger. "2") 7830457))
                                         1)))))))

;; https://projecteuler.net/problem=99
(defn- pair-to-bigint [base exp]
  (.pow (BigInteger. base) (Integer/parseInt exp)))
(defn largest-exponential-naive [& [source]]
  (let [source (or source "data/099.txt")
        data   (slurp source)
        seq    (re-seq #"(\d+),(\d+)" data)]
    (last (last (sort #(compare (first %1) (first %2))
                      (map-indexed #(list (apply pair-to-bigint (rest %2))
                                          (inc %1))
                                   seq))))))
(defn largest-exponential-naive-parallel [& [source]]
  (let [source (or source "data/099.txt")
        data   (slurp source)
        seq    (re-seq #"(\d+),(\d+)" data)]
    (last (last (sort #(compare (first %1) (first %2))
                      (pmap #(list (apply pair-to-bigint (rest %)) (first %))
                            (map-indexed #(cons (inc %1) (rest %2)) seq)))))))
(defn- pair-to-expnum [base exp]
  (* (Integer/parseInt exp) (Math/log10 (Integer/parseInt base))))
(defn largest-exponential [& [source]]
  (let [source (or source "data/099.txt")
        data   (slurp source)
        seq    (re-seq #"(\d+),(\d+)" data)]
    (last (last (sort #(compare (first %1) (first %2))
                      (map-indexed #(list (apply pair-to-expnum (rest %2))
                                          (inc %1))
                                   seq))))))
