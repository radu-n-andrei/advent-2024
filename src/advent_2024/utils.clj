(ns advent-2024.utils
  (:require [clojure.java.io :as io])
  (:gen-class)
  )

; ------------------------------------------INPUT PARSING---------------------------------------------------------------
(defn read-test-input
  "Reads an input file and stores the transformed lines into a vector
   [input] - the result is a vector containing the lines form input.
   [input transform] - the result is a vector containing the lines with transform applied
   [input transform store init] - the result is a vector constructed through store (starting from init)
                                  with transform applied on each line"
  ([input] (read-test-input input identity))
  ([input transform] (read-test-input input transform (fn [l] #(conj % l)) []))
  ([input transform store init]
   (def acc (atom init))
   (with-open [r (io/reader input)]
     (doseq [line (line-seq r)] (swap! acc (store (transform line))))
     @acc
     )
   )
  )

; ------------------------------------------COLLECTION PROCESSING-------------------------------------------------------
(defn appearance-map
  "Creates a map detailing the number of appearances of an element within the collection
   (appearance-map [1 2 1 1 1 3 2])
   {{1 4} {2 2} {3 1}}
  "
  ([v]
   (if (empty? v) {} (appearance-map (first v) (rest v) {}))
   )
  ([c v acc]
   (if (nil? c) acc
                (appearance-map (first v)
                                (rest v)
                                (let [current (acc c)]
                                  (if (nil? current)
                                    (assoc acc c 1)
                                    (assoc acc c (inc current))))))
   )
  )

(defn vector-merge
  "Merges 2 collections by applying f to pairs of corresponding indices. Stops when one collection is empty"
  ([f v1 v2 acc]
   (if (or (empty? v1) (empty? v2)) acc (vector-merge f (rest v1) (rest v2) (conj acc (f (first v1) (first v2))))))
  ([f v1 v2] (vector-merge f v1 v2 []))
  )

(defn contains-coll? [coll sub]
  (if (empty? coll) false
                    (or (= (first coll) sub) (contains-coll? (rest coll) sub))
                    )
  )

(defn coll-diff
  ([coll-ref coll sol]
   (if (empty? coll-ref) sol
                         (if (contains-coll? coll (first coll-ref)) (coll-diff (rest coll-ref) coll sol)
                                                                    (coll-diff (rest coll-ref) coll (cons (first coll-ref) sol))
                                                                    )
                         )
   )
  ([coll-ref coll] (coll-diff coll-ref coll []))
  )

(defn map-values
  "Applies f to the values of m"
  [m f]
  (reduce #(assoc %1 (%2 0) (f (%2 1))) m m)
  )

(defn frame-2
  ([l acc]
   (if (empty? (rest l)) acc (frame-2 (rest l) (conj acc [(first l) (first (rest l))])))
   )
  ([l] (frame-2 l []))
  )