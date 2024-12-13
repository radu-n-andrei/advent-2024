(ns advent-2024.day13
  (:require [advent-2024.utils :as utils])
  (:gen-class)
  )

(def input (utils/read-test-input "resources/input/day13.in"
                                  (fn [line] (map #(Integer/parseInt %) (re-seq #"\d+" line)))
                                  ))
(defn grouped-input
  ([current acc]
   (if (> current (count input)) acc
                                 (let [
                                       button-a (input current)
                                       button-b (input (+ 1 current))
                                       prize (input (+ 2 current))
                                       ]
                                   (grouped-input (+ 4 current) (conj acc [button-a button-b prize]))
                                   )
                                 ))
  ([] (grouped-input 0 []))
  )

(defn claw
  [prize-function skip-max-press-validation]
  (fn [setup]
    (let [[[a-x a-y] [b-x b-y] [ip-x ip-y]] setup
          p-x (prize-function ip-x)
          p-y (prize-function ip-y)
          press-b (/ (- (* p-y a-x) (* a-y p-x)) (- (* a-x b-y) (* a-y b-x)))
          press-a (/ (- p-x (* b-x press-b)) a-x)
          ]
      (if (and (or skip-max-press-validation (and (>= 100 press-a) (>= 100 press-a)))
               (<= 0 press-a) (<= 0 press-b) (int? press-a) (int? press-b))
        (+ press-b (* 3 press-a)) 0
        )
      ))
  )

(println "P1" (reduce + (map (claw identity false) (grouped-input))))
(println "P2" (reduce + (map (claw #(+ 10000000000000 %) true) (grouped-input))))