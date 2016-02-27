(ns projecteuler.p097)

;; https://projecteuler.net/problem=97

(defn large-non-mersenne-prime []
  (apply str
         (reverse (take 10
                        (reverse (str (+ (* 28433
                                            (.pow (BigInteger. "2") 7830457))
                                         1)))))))
