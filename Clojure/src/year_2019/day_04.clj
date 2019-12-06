(ns year-2019.day-04
  (:require [ysera.test :refer [is= is is-not deftest]]
            [ysera.error :refer [error]]))

(defn get-input []
  (as-> (slurp "src/year_2019/day_04.txt") $
        (clojure.string/split $ #"\n")
        (vec $)))


(defn digit
  {:test (fn []
           (is= (digit "100") [1 0 0])
           (is= (digit 100101) [1 0 0 1 0 1])
           )}
  [number]
  (if (string? number)
    (map (comp read-string str) number)
    (map (comp read-string str) (str number))))


(defn valid?
  {:test (fn []
           (is (valid? 111111))
           (is (valid? 122345))
           (is-not (valid? 135679))
           (is-not (valid? 223450))
           (is-not (valid? 123789))
           (is (valid? 156678))
           )}
  [number]
  (let [digits (digit number)
        zeroth (nth digits 0)
        first (nth digits 1)
        second (nth digits 2)
        third (nth digits 3)
        fourth (nth digits 4)
        fifth (nth digits 5)]
    (and (or (= zeroth first)
             (= first second)
             (= second third)
             (= third fourth)
             (= fourth fifth))
         (<= zeroth first)
         (<= first second)
         (<= second third)
         (<= third fourth)
         (<= fourth fifth))))

(defn puzzle-4a
  {:test (fn []
           (is= (time (puzzle-4a)) 1694)
           )}
  []
  (let [lower-bound (read-string (first (get-input)))
        upper-bound (read-string (second (get-input)))]
    (->> (range lower-bound (+ upper-bound 1))
         (filter valid?)
         (count))))

(defn valid-two?
  {:test (fn []
           (is-not (valid-two? 111111))
           (is (valid-two? 122345))
           (is-not (valid-two? 135679))
           (is-not (valid-two? 223450))
           (is-not (valid-two? 123789))
           (is (valid-two? 156678))
           (is (valid-two? 112233))
           (is-not (valid-two? 111234))
           (is-not (valid-two? 122234))
           (is-not (valid-two? 123334))
           (is-not (valid-two? 123444))
           (is (valid-two? 111122))
           (is-not (valid-two? 111123))
           (is-not (valid-two? 122223))
           (is (valid-two? 112222))
           (is-not (valid-two? 123333))
           (is-not (valid-two? 111112))
           (is-not (valid-two? 122222))
           )}
  [number]
  (let [digits (digit number)
        zeroth (nth digits 0)
        first (nth digits 1)
        second (nth digits 2)
        third (nth digits 3)
        fourth (nth digits 4)
        fifth (nth digits 5)]
    (and
      ;contains a pair that isn't a part of a larger group
      (or (and (= zeroth first)
               (not= first second))
          (and (= first second)
               (not= zeroth first)
               (not= second third))
          (and (= second third)
               (not= first second)
               (not= third fourth))
          (and (= third fourth)
               (not= second third)
               (not= fourth fifth))
          (and (= fourth fifth)
               (not= third fourth)))
      ;ascending order
      (<= zeroth first)
      (<= first second)
      (<= second third)
      (<= third fourth)
      (<= fourth fifth)
      )))

(defn puzzle-4b
  {:test (fn []
           (is= (time (puzzle-4b)) 1148)
           )}
  []
  (let [lower-bound (read-string (first (get-input)))
        upper-bound (read-string (second (get-input)))]
    (->> (range lower-bound (+ upper-bound 1))
         (filter valid-two?)
         (count))))