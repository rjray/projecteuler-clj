(ns projecteuler.p094)

;; https://projecteuler.net/problem=94

;; Based on the analysis and C# algorithm at:
;; http://www.mathblog.dk/project-euler-94-almost-equilateral-triangles/

(defn- check-area-with-factor [x y f]
  (let [a-times-3 (+ (* 2 x) f), area-times-3 (* y (+ x (* f 2)))]
    (if (and (> a-times-3 0)
             (> area-times-3 0)
             (zero? (mod a-times-3 3))
             (zero? (mod area-times-3 3)))
      (- a-times-3 f) 0)))

(defn almost-equilateral-triangles [& [limit]]
  (let [limit (or limit 1000000000)]
    (loop [x 2, y 1, result 0]
      (cond
       (> (- (* 2 x) 1) limit) result
       :else
       (recur (+ (* 2 x) (* 3 y))
              (+ (* 2 y) x)
              (+ result
                 (check-area-with-factor x y -1)
                 (check-area-with-factor x y +1)))))))
