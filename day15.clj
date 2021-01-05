(def input "13,16,0,12,15,1")
(def inputT "0,3,6")
(defn toInt [num] (Integer/parseInt num))

(defrecord Item [ind val])

(defn preprocess [numsStr]
    (let [nums (map toInt (clojure.string/split numsStr #","))]
        (loop [i 0 res '()]
            (if (= i (count nums))
                res
                (recur (inc i) (concat res (list (Item. i (nth nums i)))))))))

(def processed (preprocess input))
(def processedT (preprocess inputT))

(defn findLastPos [nums n]
    (loop [pos (dec (count nums))]
        (cond (= pos -1) 0
            (= (:val (nth nums pos)) n) (inc (:ind (nth nums pos)))
            :else (recur (dec pos)))))
            ;(recur (inc pos) (if (= (nth nums pos) n) (inc pos) nIndex)))))

(defn rdc [nums]
    (loop [i (dec (count nums)) seen #{} res '()]
        (if (= -1 i)
            res
            (let [actItem (nth nums i)
                  actVal (:val (nth nums i))
                  wasSeen (contains? seen actVal)]
                (recur (dec i) (conj seen actVal) (if wasSeen res (conj res actItem)))))))

(defn makeRound [nums round]
    (let [lstRnd (dec round)
          lstNum (:val (last nums))
          lstNumPos (findLastPos (drop-last nums) lstNum)
          newNum (if (<= lstNumPos 0) 0 (- lstRnd lstNumPos ))]
        (concat (rdc nums) (list (Item. (dec round) newNum)))))

(defn play [maxRound startNums]
    (loop [round (inc (count startNums)) nums startNums]
        (if (= round (inc maxRound))
            nums
            (recur (inc round) (makeRound nums round)))))

(defrecord Stt [mem val])

(defn preprocess1 [numsStr]
    (let [nums (map toInt (clojure.string/split numsStr #","))]
        (loop [i 0 res {}]
            (if (= i (- (count nums) 1))
                (Stt. res (nth nums i))
                (recur (inc i) (assoc res (nth nums i) (inc i)))))))
(def processed1 (preprocess1 input))
(def processed1T (preprocess1 inputT))

(defn makeRound1 [state round]
    (let [lstRnd round
          actNum (:val state)
          lstNumPos (get (:mem state) actNum 0) 
          newNum  (if (<= lstNumPos 0) 0 (- lstRnd lstNumPos))]
        (Stt. (assoc (:mem state) actNum round) newNum)))

(defn play1 [maxRound startNums]
    (loop [round (inc (count (:mem startNums))) state startNums]
        (if (>= round (inc maxRound))
            state
            (recur (inc round) (makeRound1 state round)))))