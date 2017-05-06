(ns projecteuler.p144)

;; https://projecteuler.net/problem=144

;; Based on the brute-force approach here:
;; http://www.mathblog.dk/project-euler-144-investigating-multiple-reflections-of-a-laser-beam/

(defn multiple-reflections []
  (loop [xA 0.0, yA 10.1, xO 1.4, yO -9.6, count 0]
    (cond
     (and (<= xO 0.01)
          (>= xO -0.01)
          (> yO 0.0))   count
     :else
     (let [slopeA     (/ (- yO yA) (- xO xA))
           slopeO     (/ (* -4.0 xO) yO)
           tanA       (/ (- slopeA slopeO) (+ 1.0 (* slopeA slopeO)))
           slopeB     (/ (- slopeO tanA) (+ 1.0 (* tanA slopeO)))
           interceptB (- yO (* slopeB xO))
           a          (+ 4.0 (* slopeB slopeB))
           b          (* 2.0 slopeB interceptB)
           c          (- (* interceptB interceptB) 100.0)
           ans-elt    (Math/sqrt (- (* b b) (* 4.0 a c)))
           ans1       (/ (+ (- b) ans-elt) (* 2.0 a))
           ans2       (/ (- (- b) ans-elt) (* 2.0 a))
           new-xO     (if (> (Math/abs (- ans1 xO))
                             (Math/abs (- ans2 xO)))
                        ans1 ans2)
           new-yO     (+ (* slopeB new-xO) interceptB)]
       (recur xO yO new-xO new-yO (inc count))))))
