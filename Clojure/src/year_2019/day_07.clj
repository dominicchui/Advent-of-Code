(ns year-2019.day-07
  (:require [ysera.test :refer [is= is is-not deftest]]
            [ysera.error :refer [error]]
            [clojure.math.combinatorics :as combo]))


(defn get-input []
  (as-> (slurp "src/year_2019/day_07.txt") $
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

(defn digit
  {:test (fn []
           (is= (digit 100101) [1 0 0 1 0 1])
           )}
  [number]
  (map (comp read-string str) (str number)))

(defn count-digits
  {:test (fn []
           (is= (count-digits 1) 1)
           (is= (count-digits 111) 3)
           )}
  [number]
  (int (inc (Math/floor (Math/log10 number)))))

(defn interpret-addressing-mode
  {:test (fn []
           (is= (interpret-addressing-mode [1002, 4, 3, 4, 33] 0 1) 33) ;addressing mode is 0
           (is= (interpret-addressing-mode [1002, 4, 3, 4, 33] 0 2) 3) ; intermediate mode is 1
           (is= (interpret-addressing-mode [1, 0, 0, 0, 99] 0 1) 1)
           (is= (interpret-addressing-mode [101, 0, 0, 0, 99] 0 1) 0)
           (is= (interpret-addressing-mode [1001, 0, 0, 0, 99] 0 1) 1001)
           )}
  [program instruction-pointer offset]
  (let [instruction (get-at-index program instruction-pointer)
        mode (if (> (count-digits instruction) (+ 1 offset))
               (nth (reverse (digit instruction)) (+ 1 offset))
               0)]
    (cond (or (= mode 0) (nil? mode))
          (get-at-index program (get-at-index program instruction-pointer offset))
          (= mode 1)
          (get-at-index program instruction-pointer offset))))

(defn run-program
  {:test (fn []
           (is= (first (run-program [1, 0, 0, 0, 99])) [2, 0, 0, 0, 99])
           (is= (first (run-program [2, 3, 0, 3, 99])) [2, 3, 0, 6, 99])
           (is= (first (run-program [2, 4, 4, 5, 99, 0])) [2, 4, 4, 5, 99, 9801])
           (is= (first (run-program [1, 1, 1, 4, 99, 5, 6, 0, 99])) [30, 1, 1, 4, 2, 5, 6, 0, 99])
           (is= (first (run-program [1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50])) [3500, 9, 10, 70, 2, 3, 11, 0, 99, 30, 40, 50])
           (is= (first (run-program [1002, 4, 3, 4, 33])) [1002, 4, 3, 4, 99])
           (is= (first (run-program [3, 0, 99] {:input [1]})) [1, 0, 99])
           (is= (last (run-program [3, 0, 4, 0, 99] {:input [5]})) [5 "done"])
           (is= (last (run-program [3, 9, 8, 9, 10, 9, 4, 9, 99, -1, 8] {:input [7]})) [0 "done"])
           (is= (last (run-program [3, 9, 8, 9, 10, 9, 4, 9, 99, -1, 8] {:input [8]})) [1 "done"])
           (is= (last (run-program [3, 9, 7, 9, 10, 9, 4, 9, 99, -1, 8] {:input [3]})) [1 "done"])
           (is= (last (run-program [3, 9, 7, 9, 10, 9, 4, 9, 99, -1, 8] {:input [100]})) [0 "done"])
           (is= (last (run-program [3, 3, 1108, -1, 8, 3, 4, 3, 99] {:input [13]})) [0 "done"])
           (is= (last (run-program [3, 3, 1108, -1, 8, 3, 4, 3, 99] {:input [8]})) [1 "done"])
           (is= (last (run-program [3, 3, 1107, -1, 8, 3, 4, 3, 99] {:input [8]})) [0 "done"])
           (is= (last (run-program [3, 3, 1107, -1, 8, 3, 4, 3, 99] {:input [-1]})) [1 "done"])
           (is= (last (run-program [3, 21, 1008, 21, 8, 20, 1005, 20, 22, 107, 8, 21, 20, 1006, 20, 31,
                                    1106, 0, 36, 98, 0, 0, 1002, 21, 125, 20, 4, 20, 1105, 1, 46, 104,
                                    999, 1105, 1, 46, 1101, 1000, 1, 20, 4, 20, 1105, 1, 46, 98, 99] {:input [3]})) [999 "done"])
           (is= (last (run-program [3, 21, 1008, 21, 8, 20, 1005, 20, 22, 107, 8, 21, 20, 1006, 20, 31,
                                    1106, 0, 36, 98, 0, 0, 1002, 21, 125, 20, 4, 20, 1105, 1, 46, 104,
                                    999, 1105, 1, 46, 1101, 1000, 1, 20, 4, 20, 1105, 1, 46, 98, 99] {:input [8]})) [1000 "done"])
           (is= (last (run-program [3, 21, 1008, 21, 8, 20, 1005, 20, 22, 107, 8, 21, 20, 1006, 20, 31,
                                    1106, 0, 36, 98, 0, 0, 1002, 21, 125, 20, 4, 20, 1105, 1, 46, 104,
                                    999, 1105, 1, 46, 1101, 1000, 1, 20, 4, 20, 1105, 1, 46, 98, 99] {:input [10]})) [1001 "done"])
           )}
  [instructions & args]
  (loop [program [instructions
                  (:input (first args))
                  (if (nil? (:output (first args))) [] (:output (first args)))] ;output starts empty
         pointer (if (nil? (:pointer (first args))) 0 (:pointer (first args)))]
    (let [instructions (first program)
          inputs (second program)
          outputs (last program)
          current-instruction (nth instructions pointer)
          num-digits (count-digits current-instruction)
          opcode (if (> num-digits 1)
                   (Integer/parseInt (subs (str current-instruction) (- (count-digits current-instruction) 2)))
                   current-instruction)]
      (cond (= current-instruction 99)
            [instructions -1 [] (conj (vec outputs) "done")]
            (and (= opcode 3) (empty? inputs))              ;waiting for more input
            [instructions pointer inputs outputs]
            :else (recur (cond (= opcode 1)
                               ;adds param 1 to param 2 and places it at location indicated by param 3
                               [(assoc instructions (get-at-index instructions pointer 3)
                                                    (+ (interpret-addressing-mode instructions pointer 1)
                                                       (interpret-addressing-mode instructions pointer 2))) inputs outputs]
                               (= opcode 2)
                               ;multiplies param 1 with param 2 and places it at location indicated by param 3
                               [(assoc instructions (get-at-index instructions pointer 3)
                                                    (* (interpret-addressing-mode instructions pointer 1)
                                                       (interpret-addressing-mode instructions pointer 2))) inputs outputs]
                               (= opcode 3)
                               ;gets value from input and puts it at position given by param 1
                               [(assoc instructions (get-at-index instructions pointer 1) (first inputs)) (drop 1 inputs) outputs]
                               (= opcode 4)
                               ;gets value at position given by param 1 and prints it
                               (do
                                 (let [output (interpret-addressing-mode instructions pointer 1)]
                                   (println output)
                                   [instructions inputs (conj (vec outputs) output)]))
                               (= opcode 5)
                               ;jump-if: if param 1 is non-zero, jump to value given by param 2
                               program
                               (= opcode 6)
                               ;jump-if-not: if param 1 is zero, jump to value given by param 2
                               program
                               (= opcode 7)
                               ;less than: if param 1 < param 2, store '1' in position given by param 3, otherwise 0
                               [(assoc instructions (get-at-index instructions pointer 3)
                                                    (if (< (interpret-addressing-mode instructions pointer 1)
                                                           (interpret-addressing-mode instructions pointer 2))
                                                      1
                                                      0)) inputs outputs]
                               (= opcode 8)
                               ;equals: if param 1 = param 2, store '1' in position given by param 3, otherwise 0
                               [(assoc instructions (get-at-index instructions pointer 3)
                                                    (if (= (interpret-addressing-mode instructions pointer 1)
                                                           (interpret-addressing-mode instructions pointer 2))
                                                      1
                                                      0)) inputs outputs]
                               :else (error "invalid opcode " opcode))
                         (cond (or (= opcode 1)
                                   (= opcode 2))
                               (+ pointer 4)
                               (or (= opcode 3)
                                   (= opcode 4))
                               (+ pointer 2)
                               (= opcode 5)
                               (if (not= (interpret-addressing-mode instructions pointer 1) 0)
                                 (interpret-addressing-mode instructions pointer 2)
                                 (+ pointer 3))
                               (= opcode 6)
                               (if (= (interpret-addressing-mode instructions pointer 1) 0)
                                 (interpret-addressing-mode instructions pointer 2)
                                 (+ pointer 3))
                               (or (= opcode 7)
                                   (= opcode 8))
                               (+ pointer 4)
                               :else (error "invalid instruction-pointer offset")))))))

(defn run-amplifier
  {:test (fn []
           (is= (last (run-amplifier [3, 15, 3, 16, 1002, 16, 10, 16, 1, 16, 15, 15, 4, 15, 99, 0, 0] [2 43])) [432 "done"])
           (is= (last (run-amplifier [3, 15, 3, 16, 1002, 16, 10, 16, 1, 16, 15, 15, 4, 15, 99, 0, 0] 0 [2 43] [])) [432 "done"])
           )}
  ([instructions input]
   (run-program instructions {:input input}))
  ([instructions pointer input output]
   (run-program instructions {:pointer pointer :input input :output output})))

(defn run-five-amplifiers
  {:test (fn []
           (is= (run-five-amplifiers [3, 15, 3, 16, 1002, 16, 10, 16, 1, 16, 15, 15, 4, 15, 99, 0, 0] [4 3 2 1 0])
                43210)
           )}
  [instructions phases]
  (let [amp-zero (run-program instructions {:input [(nth phases 0) 0]})
        amp-one (run-program instructions {:input [(nth phases 1) (first (last amp-zero))]}) ;last is output vector
        amp-two (run-program instructions {:input [(nth phases 2) (first (last amp-one))]})
        amp-three (run-program instructions {:input [(nth phases 3) (first (last amp-two))]})
        amp-four (run-program instructions {:input [(nth phases 4) (first (last amp-three))]})]
    (first (last amp-four))))

(defn find-max-thruster-signal
  {:test (fn []
           (is= (find-max-thruster-signal [3, 15, 3, 16, 1002, 16, 10, 16, 1, 16, 15, 15, 4, 15, 99, 0, 0]) [[4 3 2 1 0] 43210])
           (is= (find-max-thruster-signal [3, 23, 3, 24, 1002, 24, 10, 24, 1002, 23, -1, 23,
                                           101, 5, 23, 23, 1, 24, 23, 23, 4, 23, 99, 0, 0]) [[0 1 2 3 4] 54321])
           (is= (find-max-thruster-signal [3, 31, 3, 32, 1002, 32, 10, 32, 1001, 31, -2, 31, 1007, 31, 0, 33,
                                           1002, 33, 7, 33, 1, 33, 31, 31, 1, 32, 31, 31, 4, 31, 99, 0, 0, 0]) [[1 0 4 3 2] 65210])

           )}
  [instruction]
  (let [permutations (clojure.math.combinatorics/permutations [0 1 2 3 4])
        values (map (fn [permutation] (run-five-amplifiers instruction permutation)) permutations)
        map (zipmap permutations values)]
    (apply max-key val map)))

(defn puzzle-7a
  {:test (fn []
           (is= (time (puzzle-7a)) [[0 2 1 4 3] 43812])
           )}
  []
  (-> (get-input)
      (find-max-thruster-signal)))

(defn run-five-feedback-amplifiers
  {:test (fn []
           (is= (run-five-feedback-amplifiers [3, 26, 1001, 26, -4, 26, 3, 27, 1002, 27, 2, 27, 1, 27, 26,
                                               27, 4, 27, 1001, 28, -1, 28, 1005, 28, 6, 99, 0, 0, 5] [9 8 7 6 5]) 139629729)
           (is= (run-five-feedback-amplifiers [3, 52, 1001, 52, -5, 52, 3, 53, 1, 52, 56, 54, 1007, 54, 5, 55, 1005, 55, 26, 1001, 54,
                                               -5, 54, 1105, 1, 12, 1, 53, 54, 53, 1008, 54, 0, 55, 1001, 55, 1, 55, 2, 53, 55, 53, 4,
                                               53, 1001, 56, -1, 56, 1005, 56, 6, 99, 0, 0, 0, 0, 10] [9 7 8 5 6]) 18216)
           )}
  [instructions phases]

  (loop [amp-zero (run-amplifier instructions [(nth phases 0) 0])
         amp-one (run-amplifier instructions [(nth phases 1) (first (last amp-zero))]) ;last is output vector
         amp-two (run-amplifier instructions [(nth phases 2) (first (last amp-one))])
         amp-three (run-amplifier instructions [(nth phases 3) (first (last amp-two))])
         amp-four (run-amplifier instructions [(nth phases 4) (first (last amp-three))])]
    (if (= (last (last amp-four)) "done")
      (first (last amp-four))

      (let [amp-zero (run-amplifier (nth amp-zero 0) (nth amp-zero 1) (nth amp-four 3) (drop 1 (last amp-zero))) ;continue running with instructions, pointer, input, output
            amp-one (run-amplifier (nth amp-one 0) (nth amp-one 1) (nth amp-zero 3) (drop 1 (last amp-one)))
            amp-two (run-amplifier (nth amp-two 0) (nth amp-two 1) (nth amp-one 3) (drop 1 (last amp-two)))
            amp-three (run-amplifier (nth amp-three 0) (nth amp-three 1) (nth amp-two 3) (drop 1 (last amp-three)))
            amp-four (run-amplifier (nth amp-four 0) (nth amp-four 1) (nth amp-three 3) (drop 1 (last amp-four)))]
        (recur amp-zero amp-one amp-two amp-three amp-four)))))


(defn find-max-feedback-thruster-signal
  {:test (fn []
           (is= (find-max-feedback-thruster-signal [3, 26, 1001, 26, -4, 26, 3, 27, 1002, 27, 2, 27, 1, 27, 26,
                                                    27, 4, 27, 1001, 28, -1, 28, 1005, 28, 6, 99, 0, 0, 5]) [[9 8 7 6 5] 139629729])
           (is= (find-max-feedback-thruster-signal [3, 52, 1001, 52, -5, 52, 3, 53, 1, 52, 56, 54, 1007, 54, 5, 55, 1005, 55, 26, 1001, 54,
                                                    -5, 54, 1105, 1, 12, 1, 53, 54, 53, 1008, 54, 0, 55, 1001, 55, 1, 55, 2, 53, 55, 53, 4,
                                                    53, 1001, 56, -1, 56, 1005, 56, 6, 99, 0, 0, 0, 0, 10]) [[9 7 8 5 6] 18216])
           )}
  [instruction]
  (let [permutations (clojure.math.combinatorics/permutations [5 6 7 8 9])
        values (map (fn [permutation] (run-five-feedback-amplifiers instruction permutation)) permutations)
        map (zipmap permutations values)]
    (apply max-key val map)))

(defn puzzle-7b
  {:test (fn []
           (is= (time (puzzle-7b)) [[9 7 8 6 5] 59597414])
           )}
  []
  (-> (get-input)
      (find-max-feedback-thruster-signal)))