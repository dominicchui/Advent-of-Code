(ns year-2019.day-02
  (:require [ysera.test :refer [is= deftest]]
            [ysera.error :refer [error]]))


(defn get-input []
  (as-> (slurp "src/year_2019/day_02.txt") $
        (clojure.string/split $ #",")
        (map read-string $)
        (vec $)))

(defn get-at-index
  {:test (fn []
           (is= (get-at-index [0, 1, 2, 3, 4] 0) 0)
           (is= (get-at-index [1, 0, 0, 0, 99] 4) 99)
           (is= (get-at-index [0, 1, 2, 3, 4] 0 2) 2)
           (is= (get-at-index [1, 0, 0, 0, 99] 0 3) 0)
           )}
  ([program pointer]
   (nth program pointer))
  ([program pointer offset]
   (nth program (+ pointer offset))))

(defn process-program
  {:test (fn []
           (is= (process-program [1, 0, 0, 0, 99]) [2, 0, 0, 0, 99])
           (is= (process-program [2, 3, 0, 3, 99]) [2, 3, 0, 6, 99])
           (is= (process-program [2, 4, 4, 5, 99, 0]) [2, 4, 4, 5, 99, 9801])
           (is= (process-program [1, 1, 1, 4, 99, 5, 6, 0, 99]) [30, 1, 1, 4, 2, 5, 6, 0, 99])
           (is= (process-program [1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50]) [3500, 9, 10, 70, 2, 3, 11, 0, 99, 30, 40, 50])
           )}
  ([program]
   (loop [program program
          pointer 0]
     (let [opcode (nth program pointer)]
       (if (= opcode 99)
         program
         (recur (cond (= opcode 1)
                      (assoc program (get-at-index program pointer 3)
                                     (+ (get-at-index program (get-at-index program pointer 1))
                                        (get-at-index program (get-at-index program pointer 2))))
                      (= opcode 2)
                      (assoc program (get-at-index program pointer 3)
                                     (* (get-at-index program (get-at-index program pointer 1))
                                        (get-at-index program (get-at-index program pointer 2))))
                      :else (error "invalid opcode " opcode))
                (+ pointer 4))))))
  ([program noun verb]
   (let [program (-> program
                     (assoc 1 noun)
                     (assoc 2 verb))]
     (loop [program program
            pointer 0]
       (let [opcode (nth program pointer)]
         (if (= opcode 99)
           program
           (recur (cond (= opcode 1)
                        (assoc program (get-at-index program pointer 3)
                                       (+ (get-at-index program (get-at-index program pointer 1))
                                          (get-at-index program (get-at-index program pointer 2))))
                        (= opcode 2)
                        (assoc program (get-at-index program pointer 3)
                                       (* (get-at-index program (get-at-index program pointer 1))
                                          (get-at-index program (get-at-index program pointer 2))))
                        :else (error "invalid opcode " opcode))
                  (+ pointer 4))))))))

(defn puzzle-2a
  {:test (fn []
           (is= (first (puzzle-2a)) 3790645)
           )}
  []
  (-> (get-input)
      (process-program 12 2)))

(defn puzzle-2b
  {:test (fn []
           (is= (puzzle-2b) 6577)
           )}
  []
  (let [input (get-input)]
    (as-> (for [noun (range 100)
                verb (range 100)] [noun, verb]) $
          (map (fn [tuple] (process-program input (first tuple) (last tuple))) $)
          (filter (fn [program-result] (= (first program-result) 19690720)) $)
          (first $)
          (+(* 100 (nth $ 1)) (nth $ 2)))))