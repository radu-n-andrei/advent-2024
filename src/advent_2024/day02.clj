(ns advent-2024.day02
  (:require [advent-2024.utils :as utils]
            [clojure.string :as str])
  (:gen-class))

(def input (utils/read-test-input "resources/input/day02.in"
                                  (fn [line] (map #(Integer/parseInt %) (str/split line #"\s+")))
                                  ))

(defn safe-level?
  "Checks if a pair is safe with or without a predefined ordering"
  ([a b] (and (>= 3 (abs (- a b)))
              (not (= a b))))
  ([a b is-ascending]
   (and (or (and (< a b) is-ascending)
            (and (> a b) (not is-ascending))
            )
        (>= 3 (abs (- a b))))
   )
  )

(defn valid-record
  "Checks if a record is valid wrt ordering and level difference"
  ([coll] (if (or (empty? coll) (= (count coll) 1)) true
                                                    (let [[a b & r] coll]
                                                      (and (safe-level? a b) (valid-record b r (< a b)))
                                                      )
                                                    )
   )
  ([previous coll is-ascending]
   (if (empty? coll) true (let [[a & r] coll] (and (safe-level? previous a is-ascending) (valid-record a r is-ascending)))
                     ))
  )

(defn valid-record-tolerance-1
  "Checks if a record is valid allowing a single faulty level.
  Tolerance is upheld by keeping a record of established values (ie values that have already been verified as 'previous')
  and using them to recreate potential valid records when an unsafe level is met:
  - if a single element is established then it's possible that it is incorrect itself prompting the following checks:
    - the record without the first element
    - the record without the second element
    - the record without the third element
  - if more than one levels are established there is already a safe ordering in place prompting the following checks:
    - the record without the previously verified level (i.e. `previous` parameter)
    - the record without the current level (i.e. `(first coll)`)
  Once an unsafe level is met, each of the above verifications is made without tolerance.
  "
  ([coll] (if (or (empty? coll) (= (count coll) 1)) true
                                                    (let [[a b & r] coll]
                                                      (if (safe-level? a b) (valid-record-tolerance-1 b r (< a b) [a])
                                                                            (or (valid-record (cons a r)) (valid-record (cons b r)))
                                                                            )
                                                      )))
  ([previous coll is-ascending established]
   (if (empty? coll) true
                     (if (safe-level? previous (first coll) is-ascending)
                       (valid-record-tolerance-1 (first coll) (rest coll) is-ascending (conj established previous))
                       (if (= 1 (count established)) (or
                                                       (valid-record (cons previous coll)) ; the first level might be wrong
                                                       (valid-record (cons (first established) coll)) ; the second level might be wrong
                                                       (valid-record (concat (conj established previous) (rest coll)))) ; the third level might be wrong
                                                     (or (valid-record (concat established coll)) ; the previous level is wrong
                                                         (valid-record (concat (concat (conj established previous) (rest coll))))) ; the next level is wrong
                                                     ))))
  )


(println "P1:" (count (filter valid-record input)))
(println "P2:" (count (filter valid-record-tolerance-1 input)))
