(ns year-2019.day-05
  (:require [ysera.test :refer [is= is is-not deftest]]
            [ysera.error :refer [error]]))

(defn get-input []
  (as-> (slurp "src/year_2019/day_05.txt") $
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
    ;(println "instruction " instruction)
    ;(println "mode " mode)
    (cond (or (= mode 0) (nil? mode))
          (get-at-index program (get-at-index program instruction-pointer offset))
          (= mode 1)
          (get-at-index program instruction-pointer offset))))

(defn run-program
  {:test (fn []
           (is= (run-program [1, 0, 0, 0, 99]) [2, 0, 0, 0, 99])
           (is= (run-program [2, 3, 0, 3, 99]) [2, 3, 0, 6, 99])
           (is= (run-program [2, 4, 4, 5, 99, 0]) [2, 4, 4, 5, 99, 9801])
           (is= (run-program [1, 1, 1, 4, 99, 5, 6, 0, 99]) [30, 1, 1, 4, 2, 5, 6, 0, 99])
           (is= (run-program [1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50]) [3500, 9, 10, 70, 2, 3, 11, 0, 99, 30, 40, 50])
           (is= (run-program [1002, 4, 3, 4, 33]) [1002, 4, 3, 4, 99])
           (is= (run-program [3, 0, 99] 1) [1, 0, 99])
           (is= (run-program [3, 0, 4, 0, 99] 5) [5 0 4 0 99])
           )}
  ([program & input]
   (loop [program program
          pointer 0]
     (let [instruction (nth program pointer)
           num-digits (count-digits instruction)
           opcode (if (> num-digits 1)
                    (read-string (subs (str instruction) (- (count-digits instruction) 2)))
                    instruction)]
       ;(println "opcode " opcode)
       (if (= instruction 99)
         program
         (recur (cond (= opcode 1)
                      ;adds param 1 to param 2 and places it at location indicated by param 3
                      (assoc program (get-at-index program pointer 3)
                                     (+ (interpret-addressing-mode program pointer 1)
                                        (interpret-addressing-mode program pointer 2)))
                      (= opcode 2)
                      ;multiplies param 1 with param 2 and places it at location indicated by param 3
                      (assoc program (get-at-index program pointer 3)
                                     (* (interpret-addressing-mode program pointer 1)
                                        (interpret-addressing-mode program pointer 2)))
                      (= opcode 3)
                      ;gets value from input and puts it at position given by param 1
                      (assoc program (get-at-index program pointer 1) (first input))
                      (= opcode 4)
                      ;gets value at position given by param 1 and prints it
                      (do
                        (println (interpret-addressing-mode program pointer 1))
                        program)
                      :else (error "invalid opcode " opcode))
                (cond (or (= opcode 1)
                          (= opcode 2))
                      (+ pointer 4)
                      (or (= opcode 3)
                          (= opcode 4))
                      (+ pointer 2))))))))

(defn puzzle-5a
  {:test (fn []
           (is= (puzzle-5a) [3 225 1 225 6 6 1101 1 238 225 104 0 1101 40 71 224 1001 224 -111 224 4 224 1002
                             223 8 223 101 7 224 224 1 224 223 223 1102 66 6 225 1102 22 54 225 1 65 35 224 1001
                             224 -86 224 4 224 102 8 223 223 101 6 224 224 1 224 223 223 1102 20 80 225 101 92
                             148 224 101 -162 224 224 4 224 1002 223 8 223 101 5 224 224 1 224 223 223 1102 63
                             60 225 1101 32 48 225 2 173 95 224 1001 224 -448 224 4 224 102 8 223 223 1001 224 4
                             224 1 224 223 223 1001 91 16 224 101 -79 224 224 4 224 1002 223 8 223 101 3 224 224
                             1 224 223 223 1101 13 29 225 1101 71 70 225 1002 39 56 224 1001 224 -1232 224 4 224
                             102 8 223 223 101 4 224 224 1 223 224 223 1101 14 59 225 102 38 143 224 1001 224 -494
                             224 4 224 102 8 223 223 101 3 224 224 1 224 223 223 1102 30 28 224 1001 224 -840 224
                             4 224 1002 223 8 223 101 4 224 224 1 223 224 223 4 223 99 16434972 4 73 677 0 0 0 0
                             0 0 0 0 0 0 0 1105 0 99999 1105 227 247 1105 1 99999 1005 227 99999 1005 0 256 1105
                             1 99999 1106 227 99999 1106 0 265 1105 1 99999 1006 0 99999 1006 227 274 1105 1 99999
                             1105 1 280 1105 1 99999 1 225 225 225 1101 294 0 0 105 1 0 1105 1 99999 1106 0 300
                             1105 1 99999 1 225 225 225 1101 314 0 0 106 0 0 1105 1 99999 107 677 226 224 1002
                             223 2 223 1005 224 329 1001 223 1 223 8 226 226 224 102 2 223 223 1006 224 344 101
                             1 223 223 7 226 677 224 1002 223 2 223 1005 224 359 101 1 223 223 1007 677 226 224
                             1002 223 2 223 1005 224 374 1001 223 1 223 1007 677 677 224 1002 223 2 223 1006 224
                             389 101 1 223 223 1008 226 226 224 1002 223 2 223 1005 224 404 1001 223 1 223 108 677
                             226 224 1002 223 2 223 1006 224 419 1001 223 1 223 1108 677 226 224 102 2 223 223
                             1006 224 434 1001 223 1 223 108 226 226 224 1002 223 2 223 1005 224 449 101 1 223
                             223 7 677 677 224 1002 223 2 223 1006 224 464 1001 223 1 223 8 226 677 224 1002 223
                             2 223 1005 224 479 1001 223 1 223 107 226 226 224 102 2 223 223 1006 224 494 101 1
                             223 223 1007 226 226 224 1002 223 2 223 1005 224 509 1001 223 1 223 1107 226 677 224
                             102 2 223 223 1005 224 524 1001 223 1 223 108 677 677 224 1002 223 2 223 1005 224 539
                             101 1 223 223 1107 677 226 224 102 2 223 223 1005 224 554 1001 223 1 223 107 677 677
                             224 1002 223 2 223 1005 224 569 101 1 223 223 8 677 226 224 102 2 223 223 1005 224
                             584 1001 223 1 223 7 677 226 224 102 2 223 223 1006 224 599 101 1 223 223 1008 677
                             677 224 1002 223 2 223 1005 224 614 101 1 223 223 1008 677 226 224 102 2 223 223 1006
                             224 629 1001 223 1 223 1108 677 677 224 102 2 223 223 1006 224 644 101 1 223 223 1108
                             226 677 224 1002 223 2 223 1005 224 659 1001 223 1 223 1107 226 226 224 102 2 223 223
                             1006 224 674 1001 223 1 223 4 223 99 226])
           )}
  []
  (run-program (get-input) 1))

