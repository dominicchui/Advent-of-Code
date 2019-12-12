(ns year-2019.day-08
  (:require [ysera.test :refer [is= is is-not deftest]]
            [ysera.error :refer [error]]))

(defn get-input []
  (as-> (slurp "src/year_2019/day_08.txt") $
        (clojure.string/split $ #"\n")
        (vec $)))

(defn split-into-layers
  {:test (fn []
           (is= (split-into-layers "123456789012" 2 3) [[123 456] [789 012]]))
   }
  [input rows cols]
  (->> (map clojure.string/join (partition-all cols input))
       (map read-string)
       (partition-all rows)))

(defn count-occurrences
  {:test (fn []
           (is= (count-occurrences "1230" 0) 1)
           (is= (count-occurrences "123423409834033" 0) 2)
           (is= (count-occurrences "123423409834033" 3) 5)
           )}
  [input char]
  (->> (map (fn [x] (Character/digit x 10)) (map first (partition 1 input)))
       (filter (fn [x] (= x char)))
       (count)))


(defn find-layer-with-least-zeroes
  {:test (fn []
           (is= (find-layer-with-least-zeroes "123456789012" 2 3) "123456")
           (is= (find-layer-with-least-zeroes "912345600012" 2 3) "912345")
           )}
  [input rows cols]
  (let [layers (map clojure.string/join (partition-all (* rows cols) input))
        zeroes (map (fn [layer] (count-occurrences layer 0)) layers)
        map (zipmap layers zeroes)]
    (-> (sort-by last map)
        (first)                                             ;layer with least zeroes
        (first))))                                          ;the layer itself


(defn puzzle-8a
  {:test (fn []
           (is= (time (puzzle-8a)) 1463)
           )}
  []
  (let [layer (as-> (get-input) $
                    (first $)
                    (find-layer-with-least-zeroes $ 25 6))]
    (println layer)
    (* (count-occurrences layer 1) (count-occurrences layer 2))))