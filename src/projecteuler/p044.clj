(ns projecteuler.core)

;; https://projecteuler.net/problem=44

(defn- P [n] (/ (* n (- (* 3 n) 1)) 2))

(defn- is-pentagonal? [n]
  (let [t (/ (inc (Math/sqrt (inc (* 24 n)))) 6)]
    (and (pos? n) (== t (int t)))))

;; Avg elapsed time: 28.017s (Linux/rjray)
(defn pentagon-numbers []
  (first (sort #(compare (last %1) (last %2))
               (filter seq
                       (for [j (range 1 10000), k (range j 10000)]
                         (let [Pj (P j), Pk (P k)]
                           (if (and (is-pentagonal? (+ Pk Pj))
                                    (is-pentagonal? (- Pk Pj)))
                             (list j Pj k Pk (- Pk Pj)))))))))
