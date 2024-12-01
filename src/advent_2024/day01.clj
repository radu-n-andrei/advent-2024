(ns advent-2024.day01
  (:require [advent-2024.utils :as utils]
            [clojure.string :as str])
  (:gen-class))


(def input (utils/read-test-input
             "resources/input/day01.in"
             #(str/split % #"\s+")
             (fn [line] (let [[l r] line] (fn [acc] [(conj (acc 0) l) (conj (acc 1) r)])))
             [[] []]
             ))
(def sorted-input [(sort (map #(Integer/parseInt %) (input 0))) (sort (map #(Integer/parseInt %) (input 1)))])
(def mapped-left (utils/appearance-map (sorted-input 0)))
(def mapped-right (utils/appearance-map (sorted-input 1)))


(def P1 (reduce + (utils/vector-merge #(abs (- %1 %2)) (sorted-input 0) (sorted-input 1))))
(def P2 (reduce + (map #(let [[k v] %] (* k v (let [r (mapped-right k)] (if (nil? r) 0 r)))) mapped-left)))

(println P1)
(println P2)
