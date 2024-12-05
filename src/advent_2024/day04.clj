(ns advent-2024.day04
  (:require [advent-2024.utils :as utils]
            [clojure.string :as string]
            )
  (:gen-class)
  )

(def input (utils/read-test-input "resources/input/day04.in"))

(def grab-h-right (fn [n i line]
                    (if (>= (+ i n) (count line)) nil
                                                  (subs line (+ i 1) (+ i n 1))
                                                  )
                    ))
(def grab-h-left (fn [n i line] (if (< (- i n) 0) nil (string/reverse (subs line (- i n) i)))))

(defn grab-from-rows
  ([lines n row col rf cf acc]
   (if (= n 0) acc (grab-from-rows lines (- n 1) (rf row) (cf col) rf cf (str acc (subs (lines row) col (+ col 1)))))
   )
  ([lines n row col f] (grab-from-rows lines n row col f identity ""))
  ([lines n row col rf cf] (grab-from-rows lines n row col rf cf ""))
  )

(def grab-v-up
  (fn [n row col lines]
    (if (< (- row n) 0) nil
                        (grab-from-rows lines n (- row 1) col #(- % 1))
                        )
    )
  )

(def grab-v-down
  (fn [n row col lines]
    (if (>= (+ row n) (count lines)) nil
                                     (grab-from-rows lines n (+ row 1) col #(+ % 1))
                                     )
    )
  )

(def grab-diag-u-r
  (fn [n row col lines]
    (if (or (< (- row n) 0) (>= (+ col n) (count (lines 0)))) nil
                                                              (grab-from-rows lines n (- row 1) (+ col 1) dec inc)
                                                              )
    )
  )
(def grab-diag-u-l
  (fn [n row col lines]
    (if (or (< (- row n) 0) (< (- col n) 0)) nil
                                             (grab-from-rows lines n (- row 1) (- col 1) dec dec)
                                             )
    )
  )

(def grab-diag-d-r
  (fn [n row col lines]
    (if (or (>= (+ row n) (count lines)) (>= (+ col n) (count (lines 0)))) nil
                                                                           (grab-from-rows lines n (+ row 1) (+ col 1) inc inc)
                                                                           )
    )
  )
(def grab-diag-d-l
  (fn [n row col lines]
    (if (or (>= (+ row n) (count lines)) (< (- col n) 0)) nil
                                                          (grab-from-rows lines n (+ row 1) (- col 1) inc dec)
                                                          )
    )
  )

(def cross-check
  (fn [row col lines]
    (let [row-above (lines (- row 1)) row-below (lines (+ row 1))]
      (and
        (not (nil? (re-matches #"MS|SM" (str (subs row-above (- col 1) col) (subs row-below (+ col 1) (+ col 2))))))
        (not (nil? (re-matches #"MS|SM" (str (subs row-above (+ col 1) (+ col 2)) (subs row-below (- col 1) col)))))
        )
      )
    )
  )

(defn grab-star-count
  [lines row col]
  (count (filter #(= "MAS" %) [(grab-h-left 3 col (lines row))
                               (grab-h-right 3 col (lines row))
                               (grab-v-up 3 row col lines)
                               (grab-v-down 3 row col lines)
                               (grab-diag-u-l 3 row col lines)
                               (grab-diag-u-r 3 row col lines)
                               (grab-diag-d-l 3 row col lines)
                               (grab-diag-d-r 3 row col lines)
                               ]))

  )

(defn xmas-p1
  ([lines r acc]
   (if (empty? lines) acc
                      (let [xs (utils/all-indexes-of \X (first lines))]
                        (xmas-p1 (rest lines) (+ r 1) (+ acc (reduce + (map #(grab-star-count input r %) xs))))
                        )
                      )
   )
  )

(defn xmas-p2
  ([lines r acc]
   (if (empty? (rest lines)) acc
                             (let [as (utils/all-indexes-of \A (first lines))]
                               (xmas-p2 (rest lines) (+ r 1) (+ acc (count (filter #(and (> % 0)
                                                                                         (< % (- (count (input 0)) 1))
                                                                                         (cross-check r % input)
                                                                                         ) as)))))
                             )
   )
  )


(println "P1:" (xmas-p1 input 0 0))
(println "P2:" (xmas-p2 (rest input) 1 0))