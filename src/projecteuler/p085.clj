(ns projecteuler.p085)

;; https://projecteuler.net/problem=80

;; Based on the Python algorithm here:
;; https://blog.dreamshire.com/project-euler-85-solution/

(defn- count-to-target [target]
  (loop [x 2, y (int (/ target 6)), diff Float/MAX_VALUE, xx 0, yy 0]
    (cond
     (> x y) (list xx yy (* xx yy))
     :else
     (let [newdiff (Math/abs (float (- (/ (bigint (* x y (inc x) (inc y))) 4)
                                       target)))
           newx    (+ x 2)
           newy    (int (Math/sqrt (/ (* target 4) (+ (* newx newx) newx))))]
       (cond
        (< newdiff diff) (recur newx newy newdiff x y)
        :else            (recur newx newy diff xx yy))))))

(defn count-rectangles [& [target]]
  (let [target (or target 2000000)]
    (count-to-target target)))
