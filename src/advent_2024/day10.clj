(ns advent-2024.day10
  (:require [advent-2024.utils :as utils])
  (:gen-class)
  )

(def input (utils/read-test-input "resources/input/day10.in"))
(def max-V (count input))
(def max-H (count (input 0)))

(def mat (reduce #(conj %1 (into [] (map (fn [ch] (Integer/parseInt (str ch))) %2))) [] input))

(defn get-trailheads
  [row acc]
  (if (= row max-V) acc
                    (get-trailheads (inc row)
                                    (concat acc
                                            (map (fn [c] [0 [row c]]) (utils/all-v-indexes-of 0 (mat row)))))
                    )
  )
(def trailheads (get-trailheads 0 []))

(def in-bounds (fn [r c] (and (<= 0 r) (> max-V r) (<= 0 c) (> max-H c))))
(def valid-adjacent (fn [v r c]
                      (let [adj (filter #(in-bounds (% 0) (% 1)) [[(dec r) c] [(inc r) c] [r (dec c)] [r (inc c)]])
                            valid-values (filter #(= 1 (- ((mat (% 0)) (% 1)) v)) adj)
                            ]
                        (map #(into [] (cons (inc v) [%])) valid-values)
                        )
                      ))

(defn master-explore
  [on-done on-solution start]
  (def func (fn [points-to-check score]
              (if (empty? points-to-check) (on-done score)
                                           (let [[v [r c]] (first points-to-check)
                                                 ]
                                             (if (= 9 v) (func (rest points-to-check) (on-solution score [r c]))
                                                         (func (concat (valid-adjacent v r c) (rest points-to-check)) score)
                                                         )
                                             )
                                           ))
    )
  (fn [el] (func [el] start))
  )


(println "P1" (reduce + (map (master-explore count (fn [tm s] (conj tm s)) #{}) trailheads)))
(println "P2" (reduce + (map (master-explore identity (fn [tm s] (inc tm)) 0) trailheads)))