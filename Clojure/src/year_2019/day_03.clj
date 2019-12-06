(ns year-2019.day-03
  (:require [ysera.test :refer [is= deftest error?]]
             [ysera.error :refer [error]]
            [clojure.math.numeric-tower :as math]))


(defn get-input []
  (as-> (slurp "src/year_2019/day_03.txt") $
        (clojure.string/split $ #"\n")
        (map (fn [input] (clojure.string/split input #",")) $)
        (vec $)))

(defn get-points-from-an-instruction
  {:test (fn []
           (is= (get-points-from-an-instruction [0, 0] "R2") [[1, 0], [2, 0]])
           (is= (get-points-from-an-instruction [0, 0] "L2") [[-1, 0], [-2, 0]])
           (is= (get-points-from-an-instruction [0, 0] "U2") [[0, 1], [0, 2]])
           (is= (get-points-from-an-instruction [0, 0] "D2") [[0, -1], [0, -2]])
           (is= (get-points-from-an-instruction [1, 1] "R2") [[2, 1], [3, 1]])
           (is= (get-points-from-an-instruction [1, 1] "L2") [[0, 1], [-1, 1]])
           (is= (get-points-from-an-instruction [1, 1] "U2") [[1, 2], [1, 3]])
           (is= (get-points-from-an-instruction [1, 1] "D2") [[1, 0], [1, -1]])
           (is= (get-points-from-an-instruction [1, 1] "D10") [[1 0] [1 -1] [1 -2] [1 -3] [1 -4] [1 -5] [1 -6] [1 -7] [1 -8] [1 -9]])
           )}
  [start-point instruction]
  (let [direction (subs instruction 0 1)
        magnitude (read-string (subs instruction 1))
        x (first start-point)
        y (second start-point)]
    (cond
      (= direction "R")
      (for [distance (range 1 (+ magnitude 1))] [(+ x distance), y])
      (= direction "L")
      (for [distance (reverse (range (- 0 magnitude) 0))] [(+ x distance), y])
      (= direction "U")
      (for [distance (range 1 (+ magnitude 1))] [x, (+ y distance)])
      (= direction "D")
      (for [distance (reverse (range (- 0 magnitude) 0))] [x, (+ y distance)]))))


(defn get-points-from-instructions
  {:test (fn []
           (is= (get-points-from-instructions [0, 0] ["R2", "L2"]) #{[0, 0], [1, 0], [2, 0]})
           (is= (get-points-from-instructions [0, 0] ["L2", "U1"]) #{[0, 0], [-1, 0], [-2, 0], [-2, 1]})
           (is= (get-points-from-instructions [0, 0] ["U2", "R3"]) #{[0, 0], [0, 1], [0, 2], [1, 2], [2, 2], [3 2]})
           (is= (get-points-from-instructions [0, 0] ["D2", "D3"]) #{[0, 0], [0, -1], [0, -2] [0, -3], [0, -4], [0, -5]})
           )}
  [start-point instructions]
  (set (reduce (fn [points instruction] (concat points (get-points-from-an-instruction (last points) instruction))) [start-point] instructions))
  )

(defn manhattan-distance
  {:test (fn []
           (is= (manhattan-distance [0, 0]) 0)
           (is= (manhattan-distance [0, 2]) 2)
           (is= (manhattan-distance [-2, 0]) 2)
           (is= (manhattan-distance [3, 2]) 5)
           )}
  [point]
  (+ (math/abs (first point)) (math/abs (second point))))

(defn find-closest-intersection
  {:test (fn []
           (is= (time (find-closest-intersection ["R75", "D30", "R83", "U83", "L12", "D49", "R71", "U7", "L72"]
                                           ["U62", "R66", "U55", "R34", "D71", "R55", "D58", "R83"])) 159)
           )}
  [instruction-1 instruction-2]
  (let [line-one (get-points-from-instructions [0, 0] instruction-1)
        line-two (get-points-from-instructions [0, 0] instruction-2)
        intersections (disj (clojure.set/intersection line-one line-two) [0, 0])]
    (first (sort (map manhattan-distance intersections)))))

(defn puzzle-3a
  {:test (fn []
           (is= (time (puzzle-3a)) 870)
           )}
  []
  (find-closest-intersection (first (get-input)) (second (get-input))))

(defn get-time-of-point
  {:test (fn []
           (is= (get-time-of-point [1, 0] ["R2", "L2"]) 1)
           (is= (get-time-of-point [-2, 1] ["L2", "U1"]) 3)
           (is= (get-time-of-point [1, 2] ["U2", "R3"]) 3)
           (is= (get-time-of-point [0, -1] ["D2", "U3"]) 1)
           (error? (get-time-of-point [0, -5] ["D2", "U3"]))
           )}
  [point instructions]
  (as-> (reduce (fn [points instruction]
            (concat points (get-points-from-an-instruction (last points) instruction)))
          [[0, 0]] instructions) $
        (let [time (.indexOf $ point)]
          (if (= time -1)
            (error "invalid point")
            time))))

(defn get-total-time-of-point
  {:test (fn []
           (is= (get-total-time-of-point [3, 3] ["R8","U5","L5","D3"] ["U7","R6","D4","L4"]) 40)
           (is= (get-total-time-of-point [6, 5] ["R8","U5","L5","D3"] ["U7","R6","D4","L4"]) 30)
           )}
  [point instructions-1 instructions-2]
  (let [time (+ (get-time-of-point point instructions-1) (get-time-of-point point instructions-2))]
    (println time)
    time))

(defn find-shortest-intersection-time
  {:test (fn []
           (is= (find-shortest-intersection-time ["R8","U5","L5","D3"] ["U7","R6","D4","L4"]) 30)
           (is= (time (find-shortest-intersection-time ["R75", "D30", "R83", "U83", "L12", "D49", "R71", "U7", "L72"]
                                                 ["U62", "R66", "U55", "R34", "D71", "R55", "D58", "R83"])) 610)
           )}
  [instruction-1 instruction-2]
  (let [line-one (get-points-from-instructions [0, 0] instruction-1)
        line-two (get-points-from-instructions [0, 0] instruction-2)
        intersections (disj (clojure.set/intersection line-one line-two) [0, 0])]
    (first (sort (map (fn [intersection] (get-total-time-of-point intersection instruction-1 instruction-2)) intersections)))))

(defn puzzle-3b
  ;13698 is the answer but running this takes a looong time
  []
  (find-shortest-intersection-time (first (get-input)) (second (get-input)))
  )