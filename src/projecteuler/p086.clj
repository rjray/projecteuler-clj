(ns projecteuler.p061)

;; https://projecteuler.net/problem=86

;; Based on the C# algorithm here:
;; http://www.mathblog.dk/project-euler-86-shortest-path-cuboid/

(defn- calc-solutions [l wh]
  (let [root (Math/sqrt (+ (* wh wh) (* l l)))]
    (cond
     (not= root (float (int root))) 0
     (<= wh l)                      (int (/ wh 2))
     :else                          (+ 1 (- l (int (/ (inc wh) 2)))))))

(defn- count-solutions [l]
  (apply + (map #(calc-solutions l %) (range 3 (* l 2)))))

(defn cuboid-routes [& [target]]
   (let [target (or target 1000000)]
     (loop [count 0, l 2]
       (cond
        (>= count target) l
        :else            (recur (+ count (count-solutions (inc l))) (inc l))))))
