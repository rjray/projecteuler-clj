(ns projecteuler.p064)

;; https://projecteuler.net/problem=64

;; Based on the C# code from
;; http://www.mathblog.dk/project-euler-continued-fractions-odd-period/

(defn- odd-period? [n]
  (let [a0 (int (Math/sqrt n))]
    (if (= (* a0 a0) n)
      false
      (loop [period 0, d 1, m 0, a a0]
        (let [m      (- (* d a) m)
              d      (int (/ (- n (* m m)) d))
              a      (int (/ (+ a0 m) d))
              period (inc period)]
          (if (= a (* 2 a0))
            (odd? period)
            (recur period d m a)))))))

(defn odd-period-square-roots [& [maxn]]
  (let [maxn (or maxn 10000)]
    (->> (range 2 (inc maxn))
         (filter odd-period?)
         (count))))
