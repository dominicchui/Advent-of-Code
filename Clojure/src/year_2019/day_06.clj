(ns year-2019.day-06
  (:require [ysera.test :refer [is= is is-not deftest]]
            [ysera.error :refer [error]]))

(defn get-input []
  (as-> (slurp "src/year_2019/day_06.txt") $
        (clojure.string/split $ #"\n")
        (vec $)))

(defn update-graph
  {:test (fn []
           (is= (update-graph {} "COM)A") {:COM 0 :A 1})
           (is= (update-graph {:COM 0 :A 1} "A)B") {:COM 0 :A 1 :B 2})
           (is= (update-graph {:COM 0 :A 1} "COM)B") {:COM 0 :A 1 :B 1})
           )}
  [graph orbit]
  (let [orbit (clojure.string/split orbit #"\)")
        orbitee (keyword (first orbit))
        orbiter (keyword (last orbit))]
    (if (contains? graph orbitee)
      (assoc graph orbiter (inc (get graph orbitee)))
      (-> (assoc graph orbitee 0)
          (assoc orbiter 1)))
    ))

(defn construct-graph
  {:test (fn []
           (is= (construct-graph ["COM)A", "A)B", "B)C"]) {:COM 0 :A 1 :B 2 :C 3})
           (is= (construct-graph ["COM)A", "B)C", "A)B"]) {:COM 0 :A 1 :B 2 :C 3})
           (is= (construct-graph ["COM)A", "A)B", "B)C", "A)D"]) {:COM 0 :A 1 :B 2 :C 3 :D 2})
           )}
  [data]
  (loop [[orbit & orbits] data
         graph {:COM 0}]
    ;(println orbit orbits)
    (if orbit
      (let [orbit-vec (clojure.string/split orbit #"\)")
            parent (keyword (first orbit-vec))
            child (keyword (last orbit-vec))]
        (if (contains? graph parent)
          (recur orbits (assoc graph child (inc (get graph parent))))
          (recur (conj (vec orbits) orbit) graph)))
      graph)))

(defn count-orbits-from-graph
  {:test (fn []
           (is= (count-orbits-from-graph {:COM 0 :A 1 :B 2 :C 3}) 6)
           (is= (count-orbits-from-graph {:COM 0 :A 1 :B 2 :C 3 :D 2}) 8)
           )}
  [graph]
  (reduce (fn [sum key] (+ sum (get graph key))) 0 (keys graph)))

(defn get-total-orbits
  {:test (fn []
           (is= (get-total-orbits ["COM)A" "A)B", "B)C"]) 6)
           (is= (get-total-orbits ["COM)A", "B)C", "A)B"]) 6)
           (is= (get-total-orbits ["COM)A" "A)B", "B)C", "A)D"]) 8)
           (is= (get-total-orbits ["COM)B" "B)C" "C)D" "D)E" "E)F" "B)G" "G)H" "D)I" "E)J" "J)K" "K)L"]) 42)
           )}
  [orbits]
  (-> (construct-graph orbits)
      (count-orbits-from-graph)))

(defn puzzle-6a
  {:test (fn []
           (is= (time (puzzle-6a)) 247089))}
  []
  (-> (get-input)
      (get-total-orbits)))


(defn construct-graph-2
  {:test (fn []
           (is= (construct-graph-2 ["COM)A", "A)B", "B)C"]) {:COM {:depth 0 :parent nil}
                                                             :A   {:depth 1 :parent :COM}
                                                             :B   {:depth 2 :parent :A}
                                                             :C   {:depth 3 :parent :B}})
           (is= (construct-graph-2 ["COM)A", "B)C", "A)B"]) {:COM {:depth 0 :parent nil}
                                                             :A   {:depth 1 :parent :COM}
                                                             :B   {:depth 2 :parent :A}
                                                             :C   {:depth 3 :parent :B}})
           (is= (construct-graph-2 ["COM)A", "A)B", "B)C", "A)D"]) {:COM {:depth 0 :parent nil}
                                                                    :A   {:depth 1 :parent :COM}
                                                                    :B   {:depth 2 :parent :A}
                                                                    :C   {:depth 3 :parent :B}
                                                                    :D   {:depth 2 :parent :A}})
           )}
  [data]
  (loop [[orbit & orbits] data
         graph {:COM {:depth 0 :parent nil}}]
    (if orbit
      (let [orbit-vec (clojure.string/split orbit #"\)")
            parent (keyword (first orbit-vec))
            child (keyword (last orbit-vec))]
        (if (contains? graph parent)
          (recur orbits (assoc graph child {:depth (inc (get-in graph [parent :depth])) :parent parent}))
          (recur (conj (vec orbits) orbit) graph)))
      graph)))


(defn trace-to-COM
  {:test (fn []
           (is= (trace-to-COM {:COM {:depth 0 :parent nil}
                               :A   {:depth 1 :parent :COM}
                               :B   {:depth 2 :parent :A}
                               :C   {:depth 3 :parent :B}} :C) [:C :B, :A :COM])
           (is= (trace-to-COM {:COM {:depth 0 :parent nil}
                               :A   {:depth 1 :parent :COM}
                               :B   {:depth 2 :parent :A}
                               :C   {:depth 3 :parent :B}
                               :D   {:depth 2 :parent :A}} :C) [:C :B :A :COM])
           )}
  [graph target]
  (loop [[object & objects] graph
         trace [target]]
    (if (= (last trace) :COM)
      trace
      (let [child (first object)
            parent (get-in (last object) [:parent])]
        (if (= child (last trace))
          (recur objects (conj trace parent))
          (recur (conj (vec objects) object) trace))))))

(defn find-closest-common-ancestor
  {:test (fn []
           (is= (find-closest-common-ancestor {:COM {:depth 0 :parent nil}
                                               :A   {:depth 1 :parent :COM}
                                               :B   {:depth 2 :parent :A}
                                               :C   {:depth 3 :parent :B}
                                               :D   {:depth 2 :parent :A}} :B :D) :A)
           (is= (find-closest-common-ancestor {:COM {:depth 0 :parent nil}
                                               :A   {:depth 1 :parent :COM}
                                               :B   {:depth 2 :parent :A}
                                               :C   {:depth 3 :parent :B}} :A :C) :A)
           )}
  [graph object-one object-two]
  (let [trace-one (trace-to-COM graph object-one)
        trace-two (trace-to-COM graph object-two)]
    (first (filter (fn [object] (some (fn [elem] (= elem object)) trace-two)) trace-one))
    ))

(defn calculate-minimum-transfers
  {:test (fn []
           (is= (calculate-minimum-transfers ["COM)B"
                                              "B)C"
                                              "C)D"
                                              "D)E"
                                              "E)F"
                                              "B)G"
                                              "G)H"
                                              "D)I"
                                              "E)J"
                                              "J)K"
                                              "K)L"
                                              "K)YOU"
                                              "I)SAN"]) 4)
           )}

  [data]
  (let [graph (construct-graph-2 data)
        closest-ancestor (find-closest-common-ancestor graph :YOU :SAN)
        YOU-depth (get-in graph [:YOU :depth])
        SAN-depth (get-in graph [:SAN :depth])
        ancestor-depth (get-in graph [closest-ancestor :depth])]
    (+ (- YOU-depth ancestor-depth 1) (- SAN-depth ancestor-depth 1)
       )))

(defn puzzle-6b
  {:test (fn []
           (is= (time (puzzle-6b)) 442))}
  []
  (-> (get-input)
      (calculate-minimum-transfers)))