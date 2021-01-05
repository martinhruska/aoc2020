(def input "1007125
    13,x,x,41,x,x,x,x,x,x,x,x,x,569,x,29,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,19,x,x,x,23,x,x,x,x,x,x,x,937,x,x,x,x,x,37,x,x,x,x,x,x,x,x,x,x,17")

(def input1 "939
    7,13,x,x,59,x,31,19")
(def input2 "939
    4,5,6")

(defn process [in]
    (let [lines (vec (clojure.string/split-lines in))
           timestamp (Integer/parseInt (first lines))
           buses (map #(Integer/parseInt (clojure.string/trim %)) (filter #(not= % "x") (clojure.string/split (second lines) #",")))]
           (list timestamp buses)))

(defn solve [inp]
    (let [timestamp (first inp)
           buses (second inp)]
           (loop [cnt timestamp]
            (let [deps (map #(if (= (mod cnt %1) 0) true false) buses)
                  is_departed (some #(= %1 true) deps)
                  _ (println cnt deps is_departed)]
              (if is_departed
                (list cnt (filter #(= (first %) true) (map vector deps buses)))
                (recur (inc cnt))))
    )))

(defn process1 [in]
    (let [lines (vec (clojure.string/split-lines in))
           timestamp (Integer/parseInt (first lines))
           buses (map #(let [x (clojure.string/trim %)] (if (= "x" x) x (Integer/parseInt x))) (clojure.string/split (second lines) #","))
           prd (filter #(not= (second %) "x") (map vector (range 0 (count buses)) buses))]
           (list timestamp prd)))

(defn check [cnt buses]
(let [act (first buses)]
    (cond
        (= (count buses) 0) true
        (= act "x") (check (+ cnt 1) (rest buses))
        (= (mod cnt act) 0) (check (+ cnt 1) (rest buses))
        (not= (mod cnt act) 0) false)))

(defn solve1 [inp]
    (let [timestamp (first inp)
           buses (second inp)
           ]
        (loop [cnt 7 incre 1 actBuses buses]
            (let [
                mods (map #(= (mod (+ cnt (first %)) (second %)) 0) buses)
                actMods (map #(= (mod (+ cnt (first %)) (second %)) 0) actBuses)
                isActTrue (first actMods)
                nextIncre (if isActTrue (* incre (second (first actBuses))) incre)
                _ (println timestamp buses cnt mods (some #(= false %1) mods) incre)]
            (if (not (some #(= false %1) mods))
                cnt
                (recur (+ cnt nextIncre) nextIncre (if isActTrue (rest actBuses) actBuses)))))))

(defn findStart [start val pos]
    (loop [cnt start]
        (if (= (mod (+ cnt pos) val) 0)
            cnt 
            (recur (inc cnt)))))
(comment
(defn solve1 [inp]
    (let [timestamp (first inp)
           buses (second inp)]
        (loop [cnt 100000000000000]
            (let [
                _ (println timestamp buses cnt)])
            (if (check cnt buses)
                cnt
                (recur (inc cnt)))))))