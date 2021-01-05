(def input1 "16
    10
    15
    5
    1
    11
    7
    19
    6
    12
    4")

(def input2 "28
    33
    18
    42
    31
    14
    46
    20
    48
    47
    24
    23
    49
    45
    19
    38
    39
    11
    1
    32
    25
    35
    8
    17
    7
    9
    4
    2
    34
    10
    3")

(def input "115
    134
    121
    184
    78
    84
    77
    159
    133
    90
    71
    185
    152
    165
    39
    64
    85
    50
    20
    75
    2
    120
    137
    164
    101
    56
    153
    63
    70
    10
    72
    37
    86
    27
    166
    186
    154
    131
    1
    122
    95
    14
    119
    3
    99
    172
    111
    142
    26
    82
    8
    31
    53
    28
    139
    110
    138
    175
    108
    145
    58
    76
    7
    23
    83
    49
    132
    57
    40
    48
    102
    11
    105
    146
    149
    66
    38
    155
    109
    128
    181
    43
    44
    94
    4
    169
    89
    96
    60
    69
    9
    163
    116
    45
    59
    15
    178
    34
    114
    17
    16
    79
    91
    100
    162
    125
    156
    65")


(defn next-val [val nums]
    (let [possible
        (for [x nums
            :when (and (<= (- x val) 3) (> (- x val) 0))]
            x)]
        (apply min possible))) 

(defn find-item [nums]
(let [max (apply max nums)]
    (loop [val 0 result []]
        (if (= val max)
            (conj result val)
            (recur (next-val val nums) (conj result val))))))

(defn next-val2 [val nums]
    (let [possible
            (for [x nums
                :when (and (<= (- x val) 3) (> (- x val) 0))]
                x)]
        possible))

(def res (atom []))

(defn find-item2 [nums val actway]
(let [max (apply max nums)]
        (if (= val max)
            (swap! res conj (conj actway val))
            (map #(find-item2 nums %1 (conj actway val)) (next-val2 val nums)))))

(defn pairing [v]
    (map vector v (flatten (conj [0] v))))

(def processed (set (map #(-> % (clojure.string/trim) (Integer/parseInt)) (clojure.string/split-lines input))))
(def reversed (reverse (sort (concat (vector 0 (+ 3 (apply max processed) 3)) processed))))

(->> processed
    (cons (+ 3 (apply max processed)))
    set
    find-item
    pairing
    (map #(- (first %) (second %)))
    rest
    frequencies
    (map #(second %))
    (reduce *))

(defn bounds [processed]
    (->> processed
    (cons (+ 3 (apply max processed)))
    set
    find-item
    pairing
    (map #(- (first %) (second %)))
    rest))

; TODO finish second part, count from bottom up to assoc array - sum surroundings values

    
