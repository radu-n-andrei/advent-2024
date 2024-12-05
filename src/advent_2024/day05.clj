(ns advent-2024.day05
  (:require [advent-2024.utils :as utils]
            [clojure.string :as string])
  (:gen-class))

(def input (utils/read-test-input "resources/input/day05.in"
                                  (fn [line]
                                    (if (empty? (string/trim line)) []
                                                                    (map #(Integer/parseInt %)
                                                                         (if (not (nil? (string/index-of line "|")))
                                                                           (string/split line #"\|")
                                                                           (string/split line #",")
                                                                           ))))
                                  (fn [trans] (fn [acc]
                                                (if (empty? trans) acc
                                                                   ; the assumption is that the printing lists have over 2 numbers
                                                                   (if (= 2 (count trans)) [(conj (acc 0) trans) (acc 1)]
                                                                                           [(acc 0) (conj (acc 1) trans)]
                                                                                           ))
                                                ))
                                  [[] []]
                                  ))

(def rules (input 0))
(def books (input 1))

(defn valid-book
  [book rules-to-check]
  (if (empty? rules-to-check) true
                              (let [r (first rules-to-check)
                                    first-i (.indexOf book (first r))
                                    second_i (.indexOf book (last r))
                                    noop (or (= first-i -1) (= second_i -1))
                                    ]
                                (if noop (valid-book book (rest rules-to-check))
                                         (and
                                           (< first-i second_i)
                                           (valid-book book (rest rules-to-check))
                                           )
                                         )
                                )
                              )
  )

(def middle-sum (fn [book] (reduce + (map #(let [mid (int (Math/floor (/ (count %) 2)))]
                                             ((into [] %) mid)
                                             ) book))))

(defn can-be-first
  [n others]
  (empty? (filter #(and (= n (last %)) (utils/v-contains? others (first %))) rules))
  )

(defn fix-book
  ([book acc]
   (if (empty? book) acc
                     (let [
                           candidates (filter #(can-be-first % (into [] (remove (fn [x] (= x %)) book))) book)
                           to-add (first candidates)
                           ]
                       (fix-book (remove #(= % to-add) book) (conj acc to-add))
                       ))
   )
  ([book] (fix-book book []))
  )

(def validated (utils/map-values (group-by #(valid-book % rules) books) (fn [l] (map #(into [] %) l))))
(println "P1" (middle-sum (validated true)))
(println "P2" (middle-sum (map fix-book (validated false))))
