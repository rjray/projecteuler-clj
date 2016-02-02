(ns projecteuler.core
  (import [java.util GregorianCalendar]))

;; https://projecteuler.net/problem=19

;; See also: https://en.wikipedia.org/wiki/Determination_of_the_day_of_the_week

(def greg-cent     '[0 5 3 1])
(def greg-month    '[0 3 3 6 1 4 6 2 5 0 3 5])
(def greg-lmonth   '[6 2 3 6 1 4 6 2 5 0 3 5])
(def greg-year-rem '[0 1 2 3 5 6 0 1 3 4 5 6 1 2 3 4 6 0 1 2 4 5 6 0 2 3 4 5
                     0 1 2 3 5 6 0 1 3 4 5 6 1 2 3 4 6 0 1 2 4 5 6 0 2 3 4 5
                     0 1 2 3 5 6 0 1 3 4 5 6 1 2 3 4 6 0 1 2 4 5 6 0 2 3 4 5
                     0 1 2 3 5 6 0 1 3 4 5 6 1 2 3 4])

(defn- leap-year? [y]
  (if (and (zero? (rem y 4))
           (or (not (zero? (rem y 100)))
               (zero? (rem y 400))))
    true false))

(defn- starts-on-sunday? [y m]
  ;; Since we are only testing day 1 of each month, don't need to pass d.
  (let [month (if (leap-year? y) greg-lmonth greg-month)]
    (= 1 (mod (+ 1 (month (dec m)) (greg-year-rem (rem y 100))
                 (greg-cent (mod (quot y 100) 4)))
              7))))

;; Avg elapsed time: 0.785ms
(defn count-of-sundays []
  (reduce + (for [year (range 1901 2001) month (range 1 13)]
              (if (starts-on-sunday? year month) 1 0))))

;; For reference, mishadoff's solution
;; See https://github.com/mishadoff/project-euler/blob/master/src/project_euler/problem019.clj
(defn calendar-for [year month]
  (doto (GregorianCalendar.)
    (.set GregorianCalendar/YEAR year)
    (.set GregorianCalendar/MONTH month)
    (.set GregorianCalendar/DAY_OF_MONTH 1)))

;; Avg elapsed time: 6.794ms
(defn euler-019 []
  (reduce +
          (for [year (range 1901 (inc 2000)) month (range 1 (inc 12))]
            (let [c (calendar-for year month)]
              (if (= GregorianCalendar/SUNDAY
                     (.get c GregorianCalendar/DAY_OF_WEEK)) 1 0)))))
