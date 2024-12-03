(ns advent-2024.day03
  (:require [advent-2024.utils :as utils]
            [clojure.string :as str])
  (:gen-class))

(def input (utils/read-test-input "resources/input/day03.in"))

(def mul_pattern #"mul\(\d{1,3},\d{1,3}\)")
(def extraction_pattern #"mul\((\d{1,3}),(\d{1,3})\)")

(def eval_mul #(let [[a b] (rest (re-matches extraction_pattern %))] (* (Integer/parseInt a) (Integer/parseInt b))))
(def line-eval #(reduce + (map eval_mul (re-seq mul_pattern %))))

(defn line-do-eval
  ([lines starts-enabled sol]
   (def op-subs #(let [index (str/index-of %1 %2)] (if (nil? index) %3 (%4 index))))
   (def split-by-enablement #(if starts-enabled (str/split % #"don't\(\)") (str/split % #"do\(\)")))
   (def gather-parts (fn [parts] (if starts-enabled
                                   (cons (first parts) (map #(op-subs % "do()" "" (fn [index] (subs % index))) (rest parts)))
                                   (map #(op-subs % "don't()" % (fn [index] (subs % 0 index))) (rest parts))
                                   )))
   (def next-enabled? #(if starts-enabled (not (nil? (str/index-of (last %) "do()"))) (nil? (str/index-of (last %) "don't()"))))
   (def split-line #(let [parts (split-by-enablement %)] [(str/join (gather-parts parts)) (next-enabled? parts)]))
   (if (empty? lines) sol
                      (let [[ops next-enabled] (split-line (first lines))]
                        (line-do-eval (rest lines) next-enabled (+ sol (line-eval ops)))
                        )
                      ))
  ([lines] (line-do-eval lines true 0))
  )

(println "P1:" (reduce + (map line-eval input)))
(println "P2:" (line-do-eval input))
