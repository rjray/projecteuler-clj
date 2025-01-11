(defproject projecteuler "0.1.0-SNAPSHOT"
  :description "Solutions to Project Euler problems in Clojure"
  :url "https://projecteuler.net/"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.12.0"]
                 [org.clojure/math.combinatorics "0.3.0"]
                 [org.clojure/math.numeric-tower "0.0.5"]]
  :plugins [[lein-kibit "0.1.11"]
            [lein-pprint "1.3.2"]])
