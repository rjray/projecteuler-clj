(ns projecteuler.core)

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
