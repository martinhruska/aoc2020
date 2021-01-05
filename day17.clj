(def inputT ".#.
    ..#
    ###")

(def input "#.##....
    .#.#.##.
    ###.....
    ....##.#
    #....###
    .#.#.#..
    .##...##
    #..#.###")

(defn convertLine [line]
    (let [numLine (map #(if (= % \#) 1 0) line)]
        (map list (range 0 (count numLine)) numLine)))

(defn parse [input]
    (let [lines (map clojure.string/trim (clojure.string/split-lines input))
      convertedLines (map convertLine lines)]
    (map list (range 0 (count convertedLines)) convertedLines)))

(defn to3Cords [in]
    (let [z 0
        innerToCords (fn [y line] (reduce #(assoc %1 (list (first %2) y z) (second %2)) {} line))]
    (reduce #(merge %1 (innerToCords (first %2) (second %2))) {} in)))

(defn to4Cords [in]
    (let [z 0 d 0
        innerToCords (fn [y line] (reduce #(assoc %1 (list (first %2) y z d) (second %2)) {} line))]
    (reduce #(merge %1 (innerToCords (first %2) (second %2))) {} in)))

(defn addLists [l1 l2]
    (map #(+ (first %) (second %)) (map list l1 l2)))

(def ngh3
    (for [x (range -1 2) y (range -1 2) z (range -1 2) :when (not= x y z 0)] (list x y z)))

(def ngh4
    (for [x (range -1 2) y (range -1 2) z (range -1 2) d (range -1 2) :when (not= x y z d 0)] (list x y z d)))

(defn computeNgh [ngh cordValP next]
    (let [cord (first cordValP)
          val (second cordValP)]
          (reduce #(assoc %1 (addLists cord %2) (+ (get %1 (addLists cord %2) 0) val)) next ngh)))

(defn computeNextMap [ngh actMap]
    (reduce #(computeNgh ngh %2 %1) {} actMap))
 
(defn evalNextMap [actMap nextMap]
    (let [evalPos (fn [cord]
        (cond
            (and (= (get actMap cord 0) 1) (<= 2 (get nextMap cord) 3)) 1
            (and (= (get actMap cord 0) 0) (= (get nextMap cord) 3)) 1
            :else 0))]
    (reduce #(assoc %1 (first %2) (evalPos (first %2))) {} nextMap)))

(defn computeRounds [maxRound initMap]
    (loop [round 0 actMap initMap]
        (if (= round maxRound)
            actMap
            (recur (inc round) (->> actMap (computeNextMap ngh4) (evalNextMap actMap))))))

(defn computeActive [actMap]
    (reduce #(+ %1 (second %2)) 0 actMap))

(def cordT (->> inputT
    parse
    to4Cords))

(def cord (->> input
    parse
    to4Cords))