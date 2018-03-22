(ns projecteuler.p031)

;; https://projecteuler.net/problem=31

;; Attempt: Using dynamic programming
;; See http://users.softlab.ntua.gr/~ttsiod/euler31.html

(defn- calc-row [y coins matrix]
  (loop [x 1
         m matrix]
    (cond
      (= x (count coins)) m
      (>= y (coins x))    (recur (inc x)
                                 (assoc-in m [x y]
                                           (+ (get-in m [(dec x) y])
                                              (get-in m [x (- y (coins x))]))))
      :else               (recur (inc x)
                                 (assoc-in m [x y] (get-in m [(dec x) y]))))))

(defn coin-sums [& [target coins]]
  (let [target (or target 200)
        coins  (or coins [1 2 5 10 20 50 100 200])
        matrix (vec (cons (vec (repeat (inc target) 1))
                          (vec (repeat (dec (count coins))
                                       (vec (repeat (inc target) 0))))))]
    (loop [sum    0
           matrix matrix]
      (cond
        (> sum target) (last (last matrix))
        :else (recur (inc sum) (calc-row sum coins matrix))))))

;; For reference, the recursive solution from mishadoff
;; See https://github.com/mishadoff/project-euler/blob/master/src/project_euler/problem031.clj

(defn- select-coins [money lst]
  (if (or (empty? lst) (neg? money)) 0
      (if (= 1 (count lst)) 1
          (+ (select-coins money (butlast lst))
             (select-coins (- money (last lst)) lst)))))

(defn coin-sums-rec [& [target coins]]
  (let [target (or target 200)
        coins  (or coins [1 2 5 10 20 50 100 200])]
    (select-coins target coins)))
