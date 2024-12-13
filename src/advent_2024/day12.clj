(ns advent-2024.day12
  (:require [advent-2024.utils :as utils])
  (:gen-class))

(def input (utils/read-test-input "resources/input/day12.in"))

(def partition-h (fn [line] (utils/map-values
                              (group-by #(% 0) (map-indexed (fn [i x] [x i]) line))
                              (fn [g] (reduce #(conj %1 (%2 1)) [] g))
                              )))

(defn merge-consecutive
  [points acc]
  (if (empty? points) acc
                      (let [l (last acc)
                            p (first points)
                            ]
                        (if (nil? l) (merge-consecutive (rest points) (into [] (conj acc [p p])))
                                     (if (= 1 (- p (last l)))
                                       (merge-consecutive (rest points)
                                                          (into [] (conj (into [] (drop-last acc)) [(first l) p])))
                                       (merge-consecutive (rest points) (into [] (conj acc [p p])))
                                       )
                                     )
                        )
                      )
  )

(def merged-h (fn [partitioned] (utils/map-values partitioned #(merge-consecutive % []))))

(def as-gardening-lot
  #(let [[from to] %
         area (+ 1 (- to from))
         perimeter (- (* 4 area) (* 2 (- area 1)))
         ]
     [[from to] area perimeter]
     )
  )

(def lots-h (fn [mh] (utils/map-values mh #(map as-gardening-lot %))))

(def plot-h #(-> % partition-h merged-h lots-h))

(def plotted-horizontally (into [] (map plot-h input)))


(defn merge-into-plot
  [p plot]
  (let [[p-row [p-mc p-Mc]] p
        adj-row (filter #(= 1 (- p-row (% 0))) plot)
        total (reduce + (filter pos? (map #(+ 1 (- (min p-Mc ((% 1) 1)) (max p-mc ((% 1) 0)))) adj-row)))
        ]
    [(> total 0) (* 2 total) plot]
    )
  )

(defn merge-into-plots
  [p plots]
  (let [
        [p-row [c1 c2] pa pp] p
        merge-results (map #(merge-into-plot p %) plots)
        mr (group-by #(% 0) merge-results)
        valid-merges (mr true)
        invalid-merges (mr false)
        merged-valid (set (reduce concat (map #(% 2) valid-merges)))
        per-deduction (reduce + (map #(% 1) valid-merges))
        updated-p [p-row [c1 c2] pa (- pp per-deduction)]
        into-merged (conj merged-valid updated-p)
        ]
    (concat [into-merged] (map #(% 2) invalid-merges))
    )
  )

(defn merge-row
  [row acc]
  (if (empty? row) acc
                   (let [[k & v] (first row)]
                     (merge-row (rest row) (assoc acc k (merge-into-plots v (acc k))))
                     )
                   )
  )

(defn lets-do-this
  ([rows row-index plots]
   (if (empty? rows) plots
                     (let [
                           mapped-row (reduce concat (map (fn [e] (let [[k v] e] (map (fn [par] (concat [k row-index] par)) v))) (first rows)))
                           ]

                       (lets-do-this (rest rows) (inc row-index) (merge-row mapped-row plots))
                       )
                     ))
  ([] (lets-do-this plotted-horizontally 0 {}))
  )

(defn solution
  [sols acc]
  (if (empty? sols) acc
                    (let [[k f] (first sols)
                          sum-per-plot (map (fn [plot] (reduce (fn [a single] [(+ (a 0) (single 2)) (+ (a 1) (single 3))]) [0 0] plot)) f)
                          price-per-plot (map #(* (% 0) (% 1)) sum-per-plot)
                          price-per-plant (reduce + price-per-plot)
                          ] (solution (rest sols) (+ acc price-per-plant)))
                    )
  )
(println "P1" (solution (lets-do-this) 0))