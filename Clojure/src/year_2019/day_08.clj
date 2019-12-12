(ns year-2019.day-08
  (:require [ysera.test :refer [is= is is-not deftest]]
            [ysera.error :refer [error]]))

(defn get-input []
  (as-> (slurp "src/year_2019/day_08.txt") $
        (clojure.string/split $ #"\n")
        (vec $)))

(defn split-into-layers
  {:test (fn []
           (is= (split-into-layers "123456789012" 2 3) ["123456" "789012"])
           )}
  [input rows cols]
  (map clojure.string/join (partition-all (* rows cols) input)))


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

(defn combine-pixels
  {:test (fn []
           (is= (combine-pixels 0 2) 0)
           (is= (combine-pixels 0 1) 0)
           (is= (combine-pixels 1 0) 1)
           (is= (combine-pixels 2 0) 0)
           (is= (combine-pixels 2 1) 1)
           )}
  [pixel-one pixel-two]
  (if (= pixel-one 2)
    pixel-two
    pixel-one))

(defn combine-multiple-pixels
  {:test (fn []
           (is= (combine-multiple-pixels [2 2 1]) 1)
           (is= (combine-multiple-pixels [2 0 2]) 0)
           (is= (combine-multiple-pixels [1 2 0 2]) 1)
           (is= (combine-multiple-pixels [2 1 2 0]) 1)
           )}
  [pixels]
  (reduce (fn [combined-pixel new-pixel] (combine-pixels combined-pixel new-pixel)) pixels))


(defn combine-layers
  {:test (fn []
           (is= (combine-layers ["0222" "1122" "2212" "0000"] 2 2) [0 1 1 0])
           )}
  [layers rows cols]
  (let [vertical-list (reduce (fn [list pixel-number]
                                (conj list
                                      [(map (fn [layer] (Character/digit (nth layer pixel-number) 10)) layers)])) ;this map puts all vertically aligned pixels into one list
                              [] (range (* rows cols)))]    ;the reduce then combines all the lists together
    (map (fn [pixels] (combine-multiple-pixels (first pixels))) vertical-list))) ;combine the layers together

(defn puzzle-8b
  {:test (fn []
            (is= (time (puzzle-8b)) [[0 1 1 0 0 1 0 0 1 0 0 1 1 0 0 1 0 0 1 0 1 0 0 1 0]
                                     [1 0 0 1 0 1 0 1 0 0 1 0 0 1 0 1 0 1 0 0 1 0 0 1 0]
                                     [1 0 0 0 0 1 1 0 0 0 1 0 0 0 0 1 1 0 0 0 1 1 1 1 0]
                                     [1 0 1 1 0 1 0 1 0 0 1 0 0 0 0 1 0 1 0 0 1 0 0 1 0]
                                     [1 0 0 1 0 1 0 1 0 0 1 0 0 1 0 1 0 1 0 0 1 0 0 1 0]
                                     [0 1 1 1 0 1 0 0 1 0 0 1 1 0 0 1 0 0 1 0 1 0 0 1 0]])
            )}
   []
   (as-> (get-input) $
         (first $)
         (split-into-layers $ 25 6)
         (combine-layers $ 25 6)
         (partition 25 $)))