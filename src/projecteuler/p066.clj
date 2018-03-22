(ns projecteuler.p066)

;; https://projecteuler.net/problem=66

(defn- not-square [n]
  (let [root (int (Math/sqrt n))]
    (not= (* root root) n)))

;; This is an adaptation of the C# code from here:
;; http://www.mathblog.dk/project-euler-66-diophantine-equation/
(defn- solve-diophantine [D]
  (let [limit (int (Math/sqrt D))]
    (loop [m 0, d 1, a limit, numm1 1, num a, denm1 0, den 1]
      (cond
        (= 1 (- (* num num) (* D den den))) (list D num)
        :else
        (let [m (- (* d a) m)
              ;; It isn't clear from the C# code, but these have to be cast down
              ;; to bigint:
              d (bigint (/ (- D (* m m)) d))
              a (bigint (/ (+ limit m) d))
              numm2 numm1
              numm1 num
              denm2 denm1
              denm1 den
              num (+ (* a numm1) numm2)
              den (+ (* a denm1) denm2)]
          (recur m d a numm1 num denm1 den))))))

(defn diophantine-equations [& [maxn]]
  (let [maxn (or maxn 1000)]
    (->> (range 2 (inc maxn))
         (filter not-square)
         (map solve-diophantine)
         (sort #(compare (second %2) (second %1)))
         (ffirst))))
