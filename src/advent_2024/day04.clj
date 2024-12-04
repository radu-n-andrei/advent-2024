(ns advent-2024.day04
  (:require [advent-2024.utils :as utils])
  (:gen-class))

(def input (utils/read-test-input "resources/input/day04.in"))

(defn coords
  ([lines row acc]
   (if (empty? lines) acc
                      (coords (rest lines) (+ row 1)
                              (reduce-kv #(assoc %1 %3 (conj (%1 %3) [row %2]))
                                         acc
                                         (map-indexed #(clojure.lang.MapEntry/create %1 %2) (first lines))))
                      )
   )
  ([lines] (coords lines 0 {\X [], \M [], \A [], \S []}))
  )

(defn adjacent? [c1 c2]
  (and (<= (abs (- (c1 0) (c2 0))) 1)
       (<= (abs (- (c1 1) (c2 1))) 1)
       )
  )

(def coord-input (coords input))
(def xs (coord-input \X))
(def ms (coord-input \M))

(defn reach-and-keep
  "Returns a vector where the first element is a map of points
  from c1 to reachable points from c2
  and the second element is a set of reachable elements from c2
  "
  ([c1 c2 r]
   (if (empty? c1) r
                   (let [x (first c1) m (filter #(adjacent? x %) c2)]
                     (if (empty? m) (reach-and-keep (rest c1) c2 r)
                                    (reach-and-keep (rest c1) c2 (assoc r x m))
                                    )
                     )
                   ))
  ([c1 c2] (reach-and-keep c1 c2 {}))
  )

(def lookup-order [\X \M \A \S])

(defn gen-append
  [coll to-add]
  (map #(conj coll %) to-add)
  )

(def partially-valid-diag-only?
  (fn [l]
    (and (= 1 (count (distinct (map #(let [[[x0 y0] [x1 y1]] %]
                                       [(- x0 x1) (- y0 y1)]
                                       ) (utils/frame-2 l)))))
         (or (< (count l) 1) (and (not (= (- ((first l) 0) ((first (rest l)) 0)) 0))
                                  (not (= (- ((first l) 1) ((first (rest l)) 1)) 0))
                                  ))
         )
    )
  )

(def partially-valid?
  (fn [l]
    (or
      (= 1 (count (distinct (map last l))))
      (= 1 (count (distinct (map first l))))
      (partially-valid-diag-only? l)
      )
    )
  )

(defn xmas
  ([letters sols pv?]
   (if (empty? letters) sols
                        (let [letter (first letters)        ; letter that needs to be added
                              curr (coord-input letter)     ; current letter's coordinates
                              prev (set (map last sols))    ; set of coordinates on top of which we add the current letter
                              pairs (reach-and-keep prev curr) ; map of coord from prev to adjacent letters from curr
                              fil-sols (filter #(utils/contains-coll? (keys pairs) (last %)) sols)
                              updated-sols (reduce concat (map (fn [s] (gen-append s (pairs (last s)))) fil-sols))
                              updated-valid-sols (filter pv? updated-sols)
                              ]
                          (xmas (rest letters) updated-valid-sols pv?)
                          )
                        )
   )
  )

(def p1-xmas (xmas (rest lookup-order) (map vector xs) partially-valid?))
(println "P1:" (count p1-xmas))
(def p2-order (xmas [\A \S] (map vector ms) partially-valid-diag-only?))
(def valid-x-mas (filter #(= 2 (count (% 1))) (group-by #(% 1) p2-order)))
(println "P2:" (count valid-x-mas))
