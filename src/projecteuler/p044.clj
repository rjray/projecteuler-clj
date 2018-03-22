(ns projecteuler.p044)

;; https://projecteuler.net/problem=44

(defn- P [n] (/ (* n (dec (* 3 n))) 2))

(defn- is-pentagonal? [n]
  (let [t (/ (inc (Math/sqrt (inc (* 24 n)))) 6)]
    (and (pos? n) (== t (int t)))))

;; Avg elapsed time: 28.017s (Linux/rjray)
(defn pentagon-numbers []
  (first (sort #(compare (last %1) (last %2))
               (remove empty?
                       (for [j (range 1 10000), k (range j 10000)]
                         (let [Pj (P j), Pk (P k)]
                           (if (and (is-pentagonal? (+ Pk Pj))
                                    (is-pentagonal? (- Pk Pj)))
                             (list j Pj k Pk (- Pk Pj)))))))))
