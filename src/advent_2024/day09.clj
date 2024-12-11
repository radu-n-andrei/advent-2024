(ns advent-2024.day09
  (:require [advent-2024.utils :as utils]
            [clojure.string :as string])
  (:gen-class))

(def input (utils/read-test-input "resources/input/day09.in"))

(def max-id (/ (- (count (input 0)) 1) 2))

(def in-split (into [] (re-seq #"\d\d?" (input 0))))
(def in-mapped (reduce #(assoc %1 (first %2) (into [] (map
                                                        (fn [n] (let [s (str n)] (if (empty? s) 0 (Integer/parseInt s)))) (rest %2))))
                       {}
                       (map-indexed
                         (fn [index p] (let [[a b] p] [index a b])) in-split)))

(def atomic-index (atom in-mapped))

(defn rearrange-mem [start-index end-index index-in-final]
  (defn gather-n
    [curr-index index-to-place n score threshold]
    (if (or (= n 0) (= curr-index threshold)) [score curr-index]
                                              (let [
                                                    available (first (@atomic-index curr-index))
                                                    left-to-search (max 0 (- n available))
                                                    gathered (if (<= available n) available n)
                                                    new-score (+ score (* curr-index (reduce + (range index-to-place (+ index-to-place gathered)))))
                                                    side-effect (swap! atomic-index (fn [a] (assoc a curr-index [(- available gathered) (last (a curr-index))])))
                                                    ]
                                                (if (= 0 left-to-search) [new-score curr-index]
                                                                         (gather-n (- curr-index 1) (+ index-to-place gathered) left-to-search new-score threshold)
                                                                         )
                                                )
                                              )
    )
  (def rearrange-in (fn [index-to-gather index-to-shift index-in-final score partition-index]
                      (let [
                            n (first (@atomic-index index-to-gather))
                            spaces (last (@atomic-index index-to-gather))
                            curr-score (+ score (* index-to-gather (reduce + (range index-in-final (+ index-in-final n)))))
                            int-index-in-final (+ index-in-final n)
                            ]

                        (if (or (= n 0) (= index-to-gather index-to-shift)) [curr-score int-index-in-final]
                                                                            (let [[gathered-score new-index-to-shift]
                                                                                  (gather-n index-to-shift int-index-in-final spaces (biginteger 0) index-to-gather)
                                                                                  ]
                                                                              (if (and (not (= spaces 0)) (or (= gathered-score 0) (= index-to-gather partition-index))) [(+ curr-score gathered-score) (+ int-index-in-final spaces)]
                                                                                                                                                                         (rearrange-in (+ 1 index-to-gather) new-index-to-shift (+ int-index-in-final spaces) (+ gathered-score curr-score) partition-index)
                                                                                                                                                                         )
                                                                              )
                                                                            )
                        )
                      ))
  (rearrange-in start-index max-id index-in-final 0 end-index)

  )

(def not-using-mutable-structures-for-this
  (let [third (/ max-id 3)
        [first-score shift-1] (rearrange-mem 0 third 0)
        [second-score shift-2] (rearrange-mem (inc third) (* 2 third) shift-1)
        [third-score] (rearrange-mem (inc (* 2 third)) max-id shift-2)
        ] (+ first-score second-score third-score))
  )
(println "P1" not-using-mutable-structures-for-this)

;; inefficient P2 but i just want to move on...
(def in-mapped-2 (utils/map-values in-mapped #(conj % [])))

(def sorted-by-index (into (sorted-map-by <) in-mapped-2))

(defn move-files
  [curr-index partition-index in]
  (if (= curr-index partition-index) in
                                     (let [
                                           to-move ((in curr-index) 0)
                                           move-at (utils/find-first in #(and (>= ((% 1) 1) to-move) (< (% 0) curr-index)))
                                           ]
                                       (if (nil? move-at) (move-files (dec curr-index) partition-index in)
                                                          (move-files (dec curr-index) partition-index
                                                                      (assoc
                                                                        (assoc in
                                                                          (move-at 0)
                                                                          [((move-at 1) 0) (- ((move-at 1) 1) to-move)
                                                                           (concat ((move-at 1) 2) (repeat to-move curr-index))])
                                                                        curr-index
                                                                        [0 ((in curr-index) 1)
                                                                         (into [] (concat (repeat to-move 0) ((in curr-index) 2)))])
                                                                      ))
                                       ))
  )

(def ch1 (move-files max-id (* 2 (/ max-id 3)) sorted-by-index))
(def ch2 (move-files (* 2 (/ max-id 3)) (/ max-id 3) ch1))
(def ch3 (move-files (/ max-id 3) 0 ch2))

(println "P2" (first (reduce (fn [acc i]
                                 (let [[k [b s o]] i
                                       [score index] acc
                                       ]
                                   [
                                    (+ score (* (reduce + (range index (+ index b))) k) (reduce + (utils/vector-merge * o (range (+ index b) (+ index b (count o))))))
                                    (+ index b s (count o))
                                    ]
                                   )
                                 ) [0 0] ch3)))
