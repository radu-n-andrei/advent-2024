(ns advent-2024.day06
  (:require [advent-2024.utils :as utils])
  (:gen-class)
  )

(def input (utils/read-test-input "resources/input/day06.in"))

(defn coords-of
  ([ch area r acc]
   (if (empty? area) acc
                     (let [cs (utils/all-indexes-of ch (first area))]
                       (coords-of ch (rest area) (+ r 1) (concat acc (map (fn [n] [r n]) cs)))
                       )
                     )
   )
  ([ch] (coords-of ch input 0 []))
  )

(def start-point (first (coords-of \^)))
(def obstacles (coords-of \#))
(def max-H (count (input 0)))
(def max-V (count input))

(def walking-f {\^ [#(- % 1) identity]
                \> [identity #(+ % 1)]
                \v [#(+ % 1) identity]
                \< [identity #(- % 1)]
                })
(def turn {\^ \>, \> \v, \v \<, \< \^})
(def opposite-direction {\^ \v, \v \^, \< \>, \> \<})

(defn first-obstacle
  ([p direction obs]
   (case direction
     \^ (last (sort-by #(% 0) (filter #(and (= (% 1) (p 1)) (< (% 0) (p 0))) obs)))
     \> (first (sort-by #(% 1) (filter #(and (= (% 0) (p 0)) (> (% 1) (p 1))) obs)))
     \v (first (sort-by #(% 0) (filter #(and (= (% 1) (p 1)) (> (% 0) (p 0))) obs)))
     \< (last (sort-by #(% 1) (filter #(and (= (% 0) (p 0)) (< (% 1) (p 1))) obs)))
     ))
  ([p direction] (first-obstacle p direction obstacles))
  )

(def move-one (fn [point direction]
                (let [wf (walking-f direction)]
                  [((wf 0) (point 0)) ((wf 1) (point 1))])
                ))

(def prev-point #(if (nil? %1) nil (move-one %1 (opposite-direction %2))))


(defn coords-between
  ([p1 p2 direction acc]
   (if (= p1 p2) acc
                 (let [np (move-one p1 direction)]
                   (if (= p2 np) acc
                                 (coords-between np p2 direction (conj acc np)))
                   )
                 )
   )
  ([p1 p2 direction] (coords-between p1 p2 direction [p1]))
  )


(defn run-off-map [p direction]
  (case direction
    \^ [-1 (p 1)]
    \> [(p 0) max-H]
    \v [max-V (p 1)]
    \< [(p 0) -1]
    )
  )

(defn guard-route-keep
  ([p direction acc]
   (let [next-stop (first-obstacle p direction)
         ]
     (if (nil? next-stop) (set (concat acc (coords-between p (run-off-map p direction) direction)))
                          (guard-route-keep (prev-point next-stop direction) (turn direction) (set (concat acc (coords-between p next-stop direction))))
                          )
     ))
  ([] (guard-route-keep start-point \^ #{}))
  )

(defn cycle?
  ([curr direction obs prev]
   (let [next-obstacle (first-obstacle curr direction obs)]
     (if (nil? next-obstacle) false
                              (if (<= 0 (.indexOf prev [next-obstacle direction])) true
                                                                                   (cycle? (prev-point next-obstacle direction) (turn direction) obs (conj prev [next-obstacle direction]))
                                                                                   )
                              )
     )
   )
  ([p] (cycle? start-point \^ (conj obstacles p) []))
  )


(def guard-route (guard-route-keep))
(println "P1" (count guard-route))
(println "P2" (count (filter #(cycle? %) (utils/coll-diff guard-route [start-point]))))
