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
  "checks the merge of an horizontal plot into a previous plot
   returns a vector where:
    - 1st is a boolean showing if the current plot can extend the previous one
    - 2nd how many spaces overlap
    - 3rd the previous plot
  "
  [p plot]
  (let [[p-row [p-mc p-Mc]] p
        adj-row (filter #(= 1 (- p-row (% 0))) plot)
        total (reduce + (filter pos? (map #(+ 1 (- (min p-Mc ((% 1) 1)) (max p-mc ((% 1) 0)))) adj-row)))
        ]
    [(> total 0) (* 2 total) plot]
    )
  )

(defn merge-into-plots
  "Given a plot p and previously built plots all of the same plant type,
  Reconstruct the plant type's plot by finding plots that overlap with p and merging them together.
  Plots that do not overlap with p remain unchanged.
  Plot p is updated so that its perimeter represents its contribution to the merged plot
  "
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
  "Merge each horizontal plot in a row to previous plots.
  acc represents a map from plant type to distinct plots of that type
  "
  [row acc]
  (if (empty? row) acc
                   (let [[k & v] (first row)]
                     (merge-row (rest row) (assoc acc k (merge-into-plots v (acc k))))
                     )
                   )
  )

(defn plot-land
  "Recursively merge rows into valid plots"
  ([rows row-index plots]
   (if (empty? rows) plots
                     (let [
                           mapped-row (reduce concat (map (fn [e] (let [[k v] e] (map (fn [par] (concat [k row-index] par)) v))) (first rows)))
                           ]

                       (plot-land (rest rows) (inc row-index) (merge-row mapped-row plots))
                       )
                     ))
  ([] (plot-land plotted-horizontally 0 {}))
  )

(defn solution
  "Build the solution for P1 by reducing each plot according to the rules."
  [sols acc]
  (if (empty? sols) acc
                    (let [[k f] (first sols)
                          sum-per-plot (map (fn [plot] (reduce (fn [a single] [(+ (a 0) (single 2)) (+ (a 1) (single 3))]) [0 0] plot)) f)
                          price-per-plot (map #(* (% 0) (% 1)) sum-per-plot)
                          price-per-plant (reduce + price-per-plot)
                          ] (solution (rest sols) (+ acc price-per-plant)))
                    )
  )

(defn solution2
  "Use the plots built for P1 to find P2.

  update-row - recursively calculate each horizontal plot's contribution to the sides:
   - each distinct horizontal plot from the first row contributes 4 sides
   - every other horizontal plot will overlap at least one horizontal plot from the previous row.
     it will contribute to the overall sides according to the number of extremities (i.e. left/right)
     which are not shared with the previous row's plots (!!! in the same order min-min max-max):
       - no extremities shared? - it will contribute 4
       - 1 extremity shared? - it will contribute 2
       - 2 extremities shared? - it will contribute 0
  "
  [sols acc]
  (def update-row (fn [plot remaining acc]
                    (if (empty? remaining) (let [reduced (reduce (fn [acc s] [(+ (acc 0) (s 0)) (+ (acc 1) (s 1))]) [0 0] acc)]
                                             (* (reduced 0) (reduced 1))
                                             )
                                           (let [[row [cm cM] a op] (first remaining)
                                                 adj-row (filter #(= 1 (- row (% 0))) plot)
                                                 cm-same? (not (empty? (filter #(= cm ((% 1) 0)) adj-row)))
                                                 cM-same? (not (empty? (filter #(= cM ((% 1) 1)) adj-row)))
                                                 updated-sides (if (and cm-same? cM-same?) 0 (if (or cm-same? cM-same?) 2 4))
                                                 ]
                                             (update-row plot (rest remaining) (conj acc [a updated-sides]))
                                             )
                                           )
                    ))
  (if (empty? sols) acc
                    (let [[k f] (first sols)
                          price-per-plot (map #(update-row % % []) f)
                          price-per-plant (reduce + price-per-plot)
                          ] (solution2 (rest sols) (+ acc price-per-plant)))
                    )
  )
(def plotted-land (plot-land))
(println "P1" (solution plotted-land 0))
(println "P2" (solution2 plotted-land 0))