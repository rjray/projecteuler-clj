(ns projecteuler.p104)

;; https://projecteuler.net/problem=104

(def fib-seq-trunc
  ((fn rfib-trunc [a b]
     (lazy-seq (cons a (rfib-trunc b (mod (+ a b) 1000000000)))))
   1 1))

(def ^:private fib-idx
  (map list (iterate inc 1) fib-seq-trunc))

(defn- pandigital? [n]
  (= (sort (seq (str n))) [\1 \2 \3 \4 \5 \6 \7 \8 \9]))

; See http://www.mathblog.dk/project-euler-104-fibonacci-pandigital/
; for the explanation of this mess...
(defn- lead-pandigital? [n]
  (let [t  (- (* n 0.20898764024997873) 0.3494850021680094)
        t2 (long (Math/pow 10 (+ 8 (- t (long t)))))]
    (pandigital? t2)))

(defn pandigital-fibo-ends []
  (ffirst (filter #(lead-pandigital? (first %))
                  (filter #(pandigital? (second %))
                          (drop-while #(< (first %) 2749) fib-idx)))))
