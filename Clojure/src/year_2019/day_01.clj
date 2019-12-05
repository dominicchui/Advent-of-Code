(ns year-2019.day_01
  (:require [ysera.test :refer [is= deftest]]))

(defn get-input []
  (->> (slurp "src/year_2019/day_01.txt")
       (clojure.string/split-lines)
       (map read-string)))

(defn module-fuel
  {:test (fn []
           (is= (module-fuel 12) 2)
           (is= (module-fuel 14) 2)
           (is= (module-fuel 1969) 654)
           (is= (module-fuel 100756) 33583))
   }
  [mass]
  (int (- (Math/floor (/ mass 3)) 2)))

(defn total-fuel
  {:test (fn []
           (is= (total-fuel [12 14]) 4)
           (is= (total-fuel [12 14 1969]) 658)
           )}
  [masses]
  (reduce (fn [total individual] (+ total (module-fuel individual))) 0 masses))

(defn puzzle-1a
  {:test (fn []
           (is= (puzzle-1a) 3538016))}
  []
  (-> (get-input)
      (total-fuel)))

(defn adjusted-fuel
  {:test (fn []
           (is= (adjusted-fuel 14) 2)
           (is= (adjusted-fuel 1969) 966)
           (is= (adjusted-fuel 100756) 50346)
           )}
  [mass]
  (loop [mass mass
         total-fuel 0]
    (let [module-fuel (module-fuel mass)]
      (if (pos? module-fuel)
        (recur module-fuel (+ module-fuel total-fuel))
        total-fuel))))

(defn total-adjusted-fuel
  {:test (fn []
           (is= (total-adjusted-fuel [12 14]) 4)
           (is= (total-adjusted-fuel [12 14 1969]) 970)
           )}
  [masses]
  (reduce (fn [total individual] (+ total (adjusted-fuel individual))) 0 masses))

(defn puzzle-1b
  {:test (fn []
           (is= (puzzle-1b) 5304147))}
  []
  (-> (get-input)
      (total-adjusted-fuel)))
