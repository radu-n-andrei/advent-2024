(ns advent-2024.day08
  (:require [advent-2024.utils :as utils])
  (:gen-class))

(def input (utils/read-test-input "resources/input/day08.in"))

(def max-H (count (input 0)))
(def max-V (count input))
(def antennas (reduce concat (map-indexed (fn [row row-in]
                                            (filter #(not (= \. (% 0))) (map-indexed (fn [index v] [v [row index]]) row-in))
                                            ) input)))

(def antenna-map (utils/map-values (group-by #(first %) antennas) (fn [values] (map #(% 1) values))))
(def antenna-pairs (utils/map-values antenna-map utils/combi-2))

(def is-in-bounds? #(and (>= (% 0) 0)
                         (< (% 0) max-V)
                         (>= (% 1) 0)
                         (< (% 1) max-H)
                         ))

(defn antinodes-strict
  [a b]
  (let [diff-h (- (a 1) (b 1))
        diff-v (- (a 0) (b 0))
        ]
    (filter is-in-bounds? [[(+ (a 0) diff-v) (+ (a 1) diff-h)]
                           [(- (b 0) diff-v) (- (b 1) diff-h)]
                           ])
    )
  )

(defn propagate
  ([p fv fh acc]
   (let [new-p [(fv (p 0)) (fh (p 1))]]
     (if (not (is-in-bounds? new-p)) acc (propagate new-p fv fh (conj acc new-p)))
     )
   )
  ([p fv fh] (propagate p fv fh []))
  )

(defn antinodes-resonance
  [a b]
  (let [diff-h (- (a 1) (b 1))
        diff-v (- (a 0) (b 0))
        ]
    (conj (concat (propagate a #(+ % diff-v) #(+ % diff-h))
                  (propagate a #(- % diff-v) #(- % diff-h))
                  ) a)
    )
  )
(def all-antinodes (fn [f]
                     (utils/map-values antenna-pairs
                                       (fn [pairs] (reduce concat (map #(f (% 0) (% 1)) pairs))))))
(def solution (fn [f] (count (distinct (reduce concat (vals (all-antinodes f)))))))

(println "P1:" (solution antinodes-strict))
(println "P2:" (solution antinodes-resonance))