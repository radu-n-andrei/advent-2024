(ns advent-2024.day11
  (:require [advent-2024.utils :as utils]
            [clojure.string :as string])
  (:gen-class)
  )

(def input (utils/read-test-input "resources/input/day11.in" (fn [l] (map #(Integer/parseInt %) (string/split l #"\s")))))

(defn split-rock
  [r]
  (let [rs (str r)
        len (count rs)]
    (map #(Integer/parseInt %) [(subs rs 0 (/ len 2)) (subs rs (/ len 2))])
    )
  )

(def even-digits? #(= 0 (rem (count (str %)) 2)))

(def mem (atom {}))

(def multiply (fn [n] (fn [i] [i n])))

(defn change-rock
  [r]
  (let [[rock mult] r
        memd (@mem rock)
        ]
    (if (nil? memd)
      (let [sol (if (= rock 0) [1]
                               (if (even-digits? r) (split-rock rock)
                                                    [(* rock 2024)]
                                                    )
                               )
            side-effect (swap! mem #(assoc % rock sol))
            ] (map (multiply mult) sol))
      (map (multiply mult) memd)
      )
    )

  )


(def blink-in (map (multiply 1) (input 0)))

(def compress (fn [coll]
                (reduce-kv (fn [i k va] (conj i [k (reduce #(+ %1 (%2 1)) 0 va)])) [] (group-by #(% 0) coll))
                ))

(defn blink
  [max acc]
  (if (= 0 max) (reduce #(+ %1 (%2 1)) 0 acc)
                (blink (dec max) (compress (reduce concat (map change-rock acc))))
                )
  )



(println "P1:" (blink 25 blink-in))
(println "P2:" (blink 75 blink-in))

