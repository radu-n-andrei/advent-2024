(ns advent-2024.day07
  (:require [advent-2024.utils :as utils]
            [clojure.string :as string]
            )
  (:gen-class))


(def input (utils/read-test-input "resources/input/day07.in"
                                  (fn [line]
                                    (let [parts (string/split line #":")]
                                      [(Long/parseLong (parts 0))
                                       (map #(Long/parseLong %) (string/split (string/trim (parts 1)) #"\s"))
                                       ]
                                      )
                                    )
                                  ))

(def valid?
  (fn [target ops]
    (fn [numbers]
      (if (= 2 (count numbers))
        (reduce #(or %1 %2)
                (map #(= target (% (first numbers) (last numbers))) ops))
        (let [[a b & r] numbers]
          (reduce #(or %1 %2)
                  (map #(and
                          (<= (% a b) target)
                          ((valid? target ops) (cons (% a b) r)))
                       ops))
          )
        )
      )
    )
  )

(def p1-ops [(fn [a b] (+ a b)) (fn [a b] (* a b))])
(def p2-ops [(fn [a b] (+ a b)) (fn [a b] (* a b)) (fn [a b] (Long/parseLong (str a b)))])

(def keep-v (fn [fs in] (filter #((valid? (first %) fs) (last %)) in)))

(def p1-sol (keep-v p1-ops input))
(def p1 (reduce #(+ %1 (first %2)) 0 p1-sol))
(println "P1:" p1)

; include the new operation only for operations that didn't pass p1
(def p2-candidates (utils/coll-diff input p1-sol))
(def p2-sol (keep-v p2-ops p2-candidates))
(println "P2:" (+ (reduce #(+ %1 (first %2)) 0 p2-sol) p1))