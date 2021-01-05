(def input "0: 4 1 5
    1: 2 3 | 3 2
    2: 4 4 | 5 5
    3: 4 5 | 5 4
    4: \"a\"
    5: \"b\"
    
    ababbb
    bababa
    abbbab
    aaabbb
    aaaabbb")

(def inputR "62: 93 93
    41: 40 111 | 127 70
    95: 127 93 | 40 40
    73: 127 24 | 40 58
    121: 127 82 | 40 75
    101: 93 95
    54: 127 64 | 40 112
    76: 71 40 | 106 127
    25: 50 40 | 126 127
    21: 127 65
    111: 65 40 | 57 127
    44: 127 127 | 127 40
    120: 98 127 | 108 40
    59: 40 111 | 127 34
    63: 65 40 | 65 127
    8: 42
    106: 44 40 | 50 127
    98: 127 48 | 40 13
    1: 36 127 | 74 40
    52: 127 20 | 40 114
    114: 50 127 | 88 40
    2: 29 40 | 45 127
    57: 127 127 | 40 127
    65: 40 40
    43: 40 89 | 127 73
    42: 40 69 | 127 5
    110: 127 122 | 40 128
    83: 40 30 | 127 103
    105: 110 40 | 53 127
    19: 116 127 | 59 40
    90: 40 40 | 127 127
    75: 93 90
    11: 42 31
    20: 127 61 | 40 62
    16: 83 127 | 12 40
    33: 65 127 | 62 40
    4: 81 127 | 91 40
    10: 40 57
    18: 61 40 | 62 127
    88: 40 127 | 127 40
    102: 40 109 | 127 107
    49: 9 127 | 51 40
    81: 25 127 | 84 40
    115: 104 40 | 106 127
    125: 43 40 | 92 127
    9: 93 127 | 127 40
    130: 106 40 | 67 127
    56: 95 40 | 57 127
    34: 40 44
    77: 127 55 | 40 124
    79: 27 40 | 13 127
    80: 40 41 | 127 52
    82: 126 127 | 61 40
    61: 40 40 | 40 127
    27: 51 40 | 95 127
    94: 40 127 | 93 40
    6: 78 127 | 129 40
    24: 61 127 | 50 40
    126: 127 40
    108: 82 40 | 56 127
    29: 127 62 | 40 61
    70: 127 95
    99: 90 127 | 61 40
    127: \"b\"
    124: 15 127 | 120 40
    48: 40 65 | 127 50
    74: 50 127 | 61 40
    55: 40 66 | 127 16
    64: 40 9
    109: 127 88 | 40 95
    112: 40 94 | 127 9
    30: 44 127 | 61 40
    47: 121 127 | 130 40
    28: 27 40 | 56 127
    23: 48 40 | 97 127
    96: 40 95 | 127 65
    37: 63 40 | 7 127
    116: 123 127 | 36 40
    7: 40 44 | 127 65
    35: 40 127
    69: 40 125 | 127 72
    36: 65 127 | 94 40
    123: 127 65 | 40 57
    85: 40 126 | 127 65
    39: 9 127 | 126 40
    26: 127 35 | 40 88
    5: 22 127 | 100 40
    60: 40 90 | 127 65
    71: 9 127 | 61 40
    78: 40 82 | 127 39
    87: 21 40 | 101 127
    46: 126 127 | 44 40
    66: 127 54 | 40 37
    13: 127 61 | 40 50
    91: 40 119 | 127 99
    31: 77 40 | 105 127
    38: 60 40 | 106 127
    128: 38 40 | 2 127
    3: 46 40 | 49 127
    117: 44 127 | 95 40
    17: 14 40 | 79 127
    84: 127 65 | 40 65
    45: 90 127 | 95 40
    86: 127 18 | 40 85
    72: 40 80 | 127 4
    103: 93 62
    50: 93 127 | 40 40
    104: 35 40 | 65 127
    89: 117 127 | 46 40
    51: 127 127
    107: 90 40 | 88 127
    0: 8 11
    32: 40 102 | 127 115
    113: 96 40 | 26 127
    14: 10 40 | 24 127
    58: 50 127 | 126 40
    15: 23 127 | 87 40
    68: 1 40 | 28 127
    40: \"a\"
    119: 65 40 | 50 127
    12: 40 33 | 127 56
    118: 57 127 | 35 40
    122: 113 127 | 3 40
    67: 127 9 | 40 126
    22: 68 40 | 47 127
    92: 86 40 | 76 127
    93: 127 | 40
    129: 84 127 | 118 40
    97: 88 127 | 65 40
    100: 17 40 | 19 127
    53: 40 6 | 127 32
    
    aabbbabaabbbbababaaababbaaaaaaaa
    babaabbbaaaabbbbaababbabbabaaaabbabaabba
    abbbaabbbbaabababbaaaababbabbbba
    bbaabbabbbaaabbbbbaaaaab
    aaabbaaaaabbabbaabaababa
    bbabbabbaabbabbbbbabbaab
    aababbaabbbabaaabbababbbabaaabbbbbaabaaabbbbaaabaabbabbaaabbabaabbbaaaababbbabaaaabbabbb
    abaabbaaaaaaababbbbbaaaa
    abaababaabaaabbbabababaabbaaababbabbbbbabbbabababbabbabbabbbbaab
    bbabbababbbbbabbbabbaaba
    babbbaaababbbbababaaabbbbaabbbab
    babaabbbbbababaabbbabbaa
    aaaaaabbaaabbabaabaabaaa
    aabbbbbbaaababbaababbbaaaaabababbaabbbab
    aabbbbbabaabbabaaababbabbbaaabaa
    babbbbbabbbaababaabaabbaaabbbbbbaaaaaaaa
    aabbbbaaaaaaaabbabbbbbbaaabbaabaaabbaaaabbababba
    ababaabbbaabbbbaabbbabab
    aaabaabbaaabaaaaaaaabbba
    aabbbabbbbbababbbababaaaabbabbbb
    bbbbabaabbaabbbabababbab
    bbbababbbbaaabbbbbbaaaba
    bbbabaaaabaabbaaaababbaa
    abbaaabbbbbabbbbbabababa
    baaabbabababbabbabbaabab
    babaabbbabbaaaabbbabaabaaabaaaaaabbabbab
    ababbbabbbbbbabbaaabaaabaaaabbaaabbbaabb
    abbbaabababaaaababbaaaaaaabababaaaababbb
    bababaaabaaaabbbaaababbb
    aaabaaabbaabbabaabababbbabbbbbaababbabaa
    abbbbabbbaaaaaabababbbabbbbaabbbaabababa
    aabaaabaabaabbbbbbaabbbabaaaaaabbababbbbaaaabaaa
    babbaabbaabaababaaaaaaab
    baabbababbabababbbaabaabaabaabbaaababbaaaaaabbaa
    aabbaaaabaabbbbaabbbbaab
    bbaaabbaababaabaaababaababbabaab
    aaabbaabababbbbaaabababbabbbabbaaaabababbabaaaaaabaabaaaaaaabbbabaaaabaa
    babbbbbbbaabaaaaabababab
    abbaaaaabaaabbaaababaabbbababaaabbbababaaaabbbaabaabaaba
    aaaaaabbbbbababbaaabaaababbabbababababab
    bbbabaaabbaababbabaababaaaabbbbbaababbba
    aaabbbbabbbbabaababbaaabababbaaabaaabbaabababaabaabbbaba
    baabbabababbbbbaaababbabbabbbabb
    bbbaababbabaabbabbbaabababababbbbbabaabb
    bababbababbaabababaababbbabaabbbbbbaababbbaabbaabbaaaabaabaaaaba
    abbbaaaaaabababbbbaaabab
    bbaabbbaabaabbbaabaaabab
    aabaabababbaabaaabbabaaaabbabbaaaababbbaaaabbabbbaaaabbbbaaaabbbbaabbabb
    aababbbbababbababbaabbababaababaaaaaaaabbaaaabbabaababbbabbbabaabbaababbbbaababb
    abbaaaabbbabbabaababaaaa
    babbaaaaaaababbababaaaaa
    bbbbaabbbbaaabbabbaababa
    ababbbabaabbbabbabaababa
    abbababbaabbbbbaabbbbbabaaabbbab
    aaabbbaaababaabbaabababbababaaaaaaabaabaabbaabbb
    bbaabbabbaabbbaabbbbabba
    aaaaaabbabbbaaaabaabaabbabbbbaba
    bbbbbabbababbbaaabaaabbbbbbbabbaaaaaabbb
    abaaababaabaaababbaaaaaaaaaabaaababbbbbbbbabbaaaaabbaabbababbababaaaaababaaaaabbaaabbabbbbabbbaa
    aabbbbaabbbbbabbbbbabbaa
    bbabababaabaaaabaaaabbaa
    babbbbaabaababbbbababbaaabaaaabaaaabbbabbabbbabbbbbbbbaa
    abbbabbbabbbaaaabaaaabbbabbbbababbbbaaaabbaababbababbbba
    bbbabaaaabbbaaaaaabbbbba
    aaaaaabbaaaaaabaaabbaaba
    babbbabaaababaabbababbba
    abbbbabbbaababaabaaaaababababbab
    bbbbaabbbbababaababaabaa
    baabbaabbabbaaaabababaaaaaabbaababbaabaa
    baababaabaababbabbbbabbb
    aabbabbababaabbbabaababb
    abaabbbbaaababaaabbaabab
    babbbababbbbbabbabaabbbaababbbaabaabaaaa
    abbababbbbbbabbabaabaabbaabaabbaababaababaaabaaabbababababbaaaab
    baaabbaababbbabbabbabbbaaaaaaaababbbaabaaaabaabaababaabbbbabbaab
    baaaaaabbaaaaabbbaabaaaabaabbbabbbbababaabaaaaaabbaaaababaaaaaabaaaabbaa
    abaabbbbababbbabaabaabbbbbbaaabaaababbaaabababab
    ababaaabaabaabbbbbbbaaaa
    baaabbabbabbbaaaaababbbaaabbbaaa
    baabababbbbabbbbbbabaaab
    abbbaaababaabbbababaaaba
    babbaaabaaabbababbaaaaabbbbbbabbbbbaaabaaaababaaaaabbaba
    aabbbaaabababbabbabaaaaa
    bbaaabbbabaabaabbababbbbbbbbabbbbabbabaabbbbbaaaaaaabbab
    abaaaabbabaaaaaaaaababbb
    baababaabbbbababbbbaaaaa
    babbaaaaaabaabababaabaab
    babbbbaabaabbbbbbaaaabbabaabaaaa
    bbbbaabbaaababbabaababbb
    abbbaaaabbbabbababaabbab
    bbaaaabbbbbaababaaaabaababaabbbabbbaaababbaabaaabaababaaaababaabbbbabaabbabaaababaaaaabb
    aaabbaaaaaabaabaaabbaaaabbaaabbabababbaabbbabbba
    aaabaaabaabababbbabaabbbbbaaabbaaabbbbabbbabbbabbbbaaaab
    abbbabbbabbbbbbaaaaaabbb
    ababaababbbaaabbabababba
    aabbaabaaababbabbaaababa
    ababaaabaaaabaabbbabababbbbaabaaaaababbabbbbaabababbaababbbbabba
    babbbbbbabaaaaaaababbbba
    aaabbbbababbbaaaabaaaaba
    ababaababababaaaaabaabbbbababaabaabbbbbbbabaabbbaabababaabaabbab
    bbbbbbababaaaabbbbababba
    babaabbabbaabbbabbaaabab
    aaabaabbababaababbabbbbabaaabbba
    bababaabbaabaaaabbababaabbaabaaa
    abbbbbbaaabaabbbabaababb
    babababbbbababbbbbaabbabbaababaabbbababbbbbbababbabbababaababbaababbabaa
    bbbabbabbabbbaaabbababbbabbbaaaabbbbbbabbbbbbababbbbabba
    baaabaaaaabaabbaaaabaabbbaaabbaaabbaaaabbbbaaabbabababababababba
    ababbaaaabaaaabbabbbaaabbaabbaaaaabaabbb
    bbaabaabababbabbabbaaaabbabbbababaababaa
    bbababaababaabbbaabbbbabbbbabababababbab
    baababaabbaabbaaaaabaaaabaabbabaabaaabaabaaaaaaaaabbbbba
    aaababbbaabaaabaaababaaaabababbabbaabaaabbbaabaabaabaaababaaababbabababa
    abaaaaaabaabbbbbbababaaaaabaaabaabbbaababbbbabba
    bbababaaaababaabababbbabaaabbabbbaaaabbbbaaaababbaaababa
    baababababbbabaaaaaaabba
    aaabbabaaaabbabbaaabbbbbabaaabab
    baabbaaaabbbbabbabaabbbbbaabbabb
    abaaabbbbabbbbabaababbbaabbaaabaabaabbbaabbbbaab
    ababbabbabaabbbaabbbbbab
    bbababbbabaabababbababaababbbbaaaaaaabbaaabbaaabaaaabaaaaaaaababaaaaaaab
    aabbbbabbbabbabbbbbaaaba
    abababaaabbababbabaabaaa
    babbbbbabaaabaabbaaabbbb
    abaabbaabaabaaaabaabaaaabbaababa
    aababaabbbaabaaabbbaabba
    bbababbbaababaabbbbaabbb
    baaabaabaaaabaabbababbbb
    bbaabaabbaabbaaabaaabbbb
    aaababbaababaababbaaaaba
    babbbbbbabbbaababaaabbba
    babaabbbaababaababaabbab
    aaaabbbbababaababbaaabaa
    aaaaaabababbbbaababbaaababbabbbb
    aaabaaaaaabbabaabbaabaaa
    aaaaaabaaaababaaabaaabaa
    baaabaaaaabaabbbbbaaabba
    abaaaaababaaaabaaaabbbbaaabbbbaaaabaabbaabaababbbabbbaab
    baaaaaabbaaaabaaaaaaaabababababbaabbaababababbbbaaaaabaaabbbbbba
    aaababaabbaaabaabbabbbabbbaabaaa
    bbaaabaaaaaaaaaaaaaaaaababbbabaa
    bbbbbaaabbbaaaaabbbaabba
    baaabbabbaaaaaabaababbaaabaababaaaaaababaaaaabaababbabba
    bbbabbbbbbaabbabaaaabbba
    aabaabbbabbbbabbaababbaabaaaabaaaabbbbaaaaabbbaaaaabbbba
    abaaaaaabaabbaabbbaaabbbbabaaaaa
    aaabbbbbbaabbbbabbbabaab
    baabbaabaaabaabaabaaaaaaabababba
    aababbabaaabaabbabbbabba
    aababaababbbbabbaabaabbaaabababaaaabbbab
    abbaaabbbbbbabbabbaaaabbbbaaabbbbbaaaaabbaabbaaabbaababb
    aabaabbbababaabaabaabbbbaabbaabb
    bbbabaabaaabbaabbbbaabbbbaabaaabbbabbbbabbaaaaabbaababaaaaabbaaabbaaaaaa
    baaaaaabbabbbbbbbbaabbabaabbbaab
    bbbabbabaaaabbbaaabaabaabaaabababbbabbbbababaaba
    aaaaaabbaabaaabaabaababb
    abbbaababaabbabaababaaaa
    babbaaabbbbbababbabaabba
    abbabbbaaaaabaaaaabbababbbbbbaabbaabbaaaaaabbabbbbbaaaabaabbbaababaaabab
    aaabbbbbbaaabbaabbabaaab
    aaabbabaaaabaabbaabaaabb
    bbabbbaaaabaabbababaabbaaabababaaabbbaab
    aabbaabaabbaaabaababbababbbaabaababbaababbbbaaba
    bbababbbbaaaabbbaabaababbaaaabab
    baababaabbaaababbbaabaaaabaabbbaabbbaaaaaaabbabababaabbbbababbabbbaaabbb
    babaaaabababbaabbbbaaaab
    bbababbbbaabbbbbbabbabab
    ababbbaabbbaababaaaaaaab
    aababbabbabbbaaababbaaaababbabbbbbaababb
    aaabbaaaaabbabbaaabbabaaabbaabaa
    abbaaabaabaaaabbaabaababbabbbaab
    aaabbaaababbaabababbabba
    aabbaabaaaabbbbbaabbbaba
    abbaaabbbaaaabbabbabbbba
    baaaabbaaababaababababab
    bababaababbbbbbaabbabaaa
    abbaaaabbbabbababbbaabaaabbbbabbaaabbbaa
    bbabbbaaaabbbbaaaaaaaaaa
    babbbbbbaaabbabaabbabbbb
    abbaaabbbbabbabbaaabbaabaaaaaabbbbbbbbabbaabbaab
    bbbbabaababbbaaabbaaabbbaabbaababaabaaba
    baabbbabbbbbbbbabbbabaabaabaaabb
    aababaabaaabbbbabbbbbaaa
    babababbaabaababbaaaaaaa
    bbbaabaabbbaaaaabbabbbba
    abbabaabaabaaaaabaaaabbbbaaabaaaaaaaabbaaababbbbbbaabbaabbbbaaabbbbbabbb
    baabbaabbabbbbaabbaabbaabbabbbaabaabbabb
    baaabaabbabbabbbaaaaaaaabbaabaaaaaababbb
    baabaabbbbbabaaabbabaabbbbbabaabbbbbabbaaaababbb
    bbbbbbabbaababbaaaabbaaabaababbb
    babaabbbaabbbbaabababbbbbbbaabbaaaaaaaab
    aabaaaabbabbaabbabbaabbbaaaabbabaabababa
    aaaaaabaaabbabaabbaabaabaaaabaab
    aaabbbbaaaababaabaaabbaaaaaaaabbbabbbaab
    abbbbabbaabbbabbabbbaababbbababaabbabaab
    bbabbbaabaabbaababaaabab
    aababaabaaabbbbbbabaabaa
    aaababbaabbaaabbbaabbbab
    baabbbbaabbabaababbababbbbabaaabbbbbabaabbaabbaabaababaabbbbabbbbaabbaba
    bbaaabbbbabaabbababbbbbbbaabbbbabbbaaaabaaaabbba
    bbabababbabbbbbaaaaaabbaaaaabbab
    bbaabaabbaabbbbbbbaabaabbabbaabaaabababa
    ababbabbaabbbbbbbbabbbbb
    aaaaabbbbbabaabbabbabbabbababbaa
    aabbabaaaabbaabaabaaaaab
    ababbbabaabaaaababaaabbbbaabbbbabbbbaaba
    ababbabbaabaabbabaabbbbbaaaaaabbaabaaabb
    bbbbbbabbaabbaababbabaaa
    abbbaaaaaaabbababaabbaabbaabaabbaaaabaaa
    baaaaaabbbaaaabbabaabbbabaabababbabaaaabbaabbbbbbabbbabb
    bbaabbbabbbabbaabbaabbbaababababbbababaaaababbbbbaabbbbaababaaababaaabbabababaaaabbbaaaa
    aabbbbaabbbaabaababaababbabbabaaabbbbaabbbabaabb
    bbababaaaaabbabaaabaaabb
    ababababbabbbabababbbaaababababaaaababaaabbbabbbababbbaabaabbbaa
    baaaabaaaaaaababbbabbbbabbaaabaaabaabaabaaababbbbaaabbbbaabaaaba
    baabbaaaaabababbbaabaaaababaabaaaaabababbbabbaaa
    bbbaaaabbbaaaaabbaabbbabaabbaabbabbbabab
    baabbbbbaababaaaabbbabaaabbbbaaaabababba
    ababbaababbbaaaaaaaaabbb
    abbbabbbabbbabaaabbaaabbbbbabaaabbaaaaba
    aaabaabaaaabbabaabbaabaa
    aabababaabbabababaaaaabbbbbbaabbaabbbbbbabbaabbbbaabaaabbbbbbaabbbaabbbb
    baabbbaabaabbabaabababaaabaaaaab
    abababaabbabababbbbaabba
    aaabbaaabaabaaabababbaababbababa
    aaabbbbababbbababaaababb
    babbaaaaaaabbabaaabbaaabbbabbbbb
    baabbaaabbaabbabaaabbabbaaabbbbabaabbbbbabbbbbaaaaaaababbabbbabb
    bbbbaaabbaabaaabbababbababaababaaabaabaa
    baabbbbababbbabababbbaab
    babbbbaaaabaabababbabbbb
    abababaababaaababaabaaabbbbbbabbbbababbabbababaabbbaaaaa
    aaabaabababbbbabbbaaabbaaaaabaabbbbaababbaaaaaaa
    bbaaabbbaaababbabbabaabb
    bbbaabaaaabababbababaaaa
    baababaaaabbabbaababababaaabbaaaabbaabbbbbaaaaba
    aabaaabaaaaabaabaaabaabbbaaaaaaababbaaba
    abababaaabaaabaaabaababa
    babbaabbababaabbaaabbbaa
    abbaaaaabaaabbaabbbaabba
    bbabaaaabaabaaabbbbbaabbabbabababaaababbbaabbaabbbabbabaaabbabaaababbabbbabbbabb
    ababbabbaaababbabababbbbbbabaabbbbbbabbaaabbbaab
    aabababaababbabbbaaababaaabbabbaabbbbbaabbabbbbaaaababbababbbbba
    aabbabbbbbaaabbabaaabbba
    ababbaababbaaaaabbabababbbaabbaa
    bbbabbbbbbabababaaabbbba
    abbaaababababbaabaaaaabb
    aabbababbbababababbaabab
    bbaabbabaababbbaabababab
    baabababaabbabbabbabbbab
    bbababaaaabbbbbbbbabbbba
    ababaababbbaaabbbabaabaa
    baaaabbbbaabbaaaabbbaaabbabbbabbabaabbab
    aabbbbbbabbaababbabbbaaabbbaabbbabaababa
    baababaabababaaababbbbabbaaababa
    babababbbaabbbbbbbaaabaa
    babbbaabbaaaababbabaabaabbbaaaaaaaababaa
    aabbbbbbbabbbbbbabaabbab
    aaaabbbbaabaabababbbabba
    aabaababaabaababbbbaaaab
    abaabbbbababbaababbbbbbaabababab
    babbbbbbabbababbaababbbb
    abbabbbabaabbbaabbabbbbaaabbbaaaabbababa
    bababbabaaabababaabbabaaabababbbabbaaaababbbabbaaababaabababbbaaaaabbaaa
    aabbababaabbaababbaaabbabbabbbaaabaaaabbaaaabaaaaabbbaaa
    babbbbbbaabbaaaabaabbbab
    baabaaaabbaaaabbabaaaabbabaaabbbaabbbbaababbbabb
    bbaabbabbaabbaaabaaaabaa
    bababaababaaaaaaaabaababaabbabaababbbbbabbbabaabbbbbbaab
    baababbabaababbabaaaaabb
    baabbbaaaaaabbbabaaaaaaaaabbbaabbaabaabaabaababa
    aabbaabaaababbbabbbabbabaababbbbabbbaabb
    abbbbababbabbaabaabaabaaaaaabbbbaababbbaaabbabbb
    abaaabbbabbababbaabbbaaa
    bbabbabaaababbbaabbbaabaaabbbbbbabbabaaabbbbbabababbbabb
    aabaababbbbaabaaaaaabbbbabbaaaaabaababbbbababbbaabaababababaabaa
    aabaabaabbbababaabbbabbaaababbbb
    aaaaabbabbababaaabaababaaaaaabaaaababbba
    bbaaaaababbbbababaabababaababbabbbabbbabbbaaabbaaaabbaababbbaababbaabbaabbbaabaa
    babaabbbbbabbabababbaaaabbbabbaa
    aaabaaaaaaaababaabbbbbbaaaaabbbabbbaabbaaaabbbaa
    babbbbabababaaabaabbbaaa
    bbbbaaabbbbbbabbbbaabbabaabbabababaabaab
    bbabbbbbbbaabbbbbaaaaabbbbbababbbaaababbabaababbabbabbaabaabaaab
    abbabbbaaaabbabbabbbaabaaabaaabbabbbbbaa
    aaabbaaaabababaaaaababbaaababbbabbbbababaaaaabab
    abaabbaaaaababbabababbbb
    aaabaabbaababaaaabbabbbabaaabbbbbbbbbaba
    bbbbaaabbbabbabaabbbbaaa
    ababaaabbbababababbbabba
    bbbabbbbaaaaaabbaaaababa
    baabbbbbbbbbaaabbababbaa
    aaabbbabbababababaaaaaaa
    baabbbabbababbabaaabbaababbaaabbabbbbbab
    bbabbababaabababbbaaabaa
    bbabbabaaabbaababbabaababbbaabbbabaaaaba
    abaabbaaabbaaabaabbaaaaa
    baabbbbaaaabbbabbaabbbbbababbbaabbbbbbbbbbaabaababaabaab
    aabbabaababaabbaabbbabba
    abbbabaabababaabababaaaa
    bbaabbabbbabbbaabbbaabbb
    baaabbabaabbabbaabbbabbbabbabbbabaabbabababbabbbaabbaaabbbabaaaaaabababababababa
    aabaaaabbbbabaababbaabbbaaabbaaababbbbabaaaabbbaaabbbbabbaabaabb
    abaabaaabababbabaaaababb
    babaabbbaaababbbbbabaaba
    abbbbabbbaabbbbaabbabaaa
    aabbabaabaabbbaababbabaa
    babbbbbbbaabaaababaaabab
    ababbaaabbaabbaababababbbbababbbbbbbabbbaaaaaaab
    baaaaabaababbbabbabaaaaaaababbabaaababbabbababbbbabaabba
    baaaaabaaabababbbabaaaaa
    bbaaabbababbbbbbaaabaaabbaaaabab
    aaaaaababbabababbbaababa
    abbabbbabaabaaabbbbbbbba
    abbbbabbbabbbabaabaaabbbabbbbabbaaabbbabaaaaabba
    bbbabaabbabbaababaaaabaabaabbabbaababbbabbaaaaabbbbaababaaababbb
    bbaaaabbabababbbbbbbbaab
    baabaaabbabbaabbaabababa
    babbbbbabaabaabbbabaaaaa
    babbbaaaababaabbabbaaaaaaabbbbab
    baaabbabbaabaabbabbbaaaaaaaaaabbaaabbbbbbabaabab
    ababaaaaaaaaaaaaababaaaaabaaabab
    bbbbbbabaababaabbbaabaabbbababba
    bababaaaaaabaabbbabababa
    bbabababaaabbabaaabbababbbbababb
    baabbabaabaabbbabbbaababaaabbabbbabbabbabbabaaaa
    abbbabbaabbaabbabbbaabbbbabbbbbaaabbbaabaababbababaaababababaaabbbabbbab
    ababbaabbabbaabaabbbbbbbbabaabaabbaaaaabbabababb
    abababaabaaaaababaababaaabbaaababbbbabbbbbabaabbabaaababaabbaabbaabaaabb
    ababbaababbaaaaabaabbbbbaaaaabaaabbababbbababbbababbaaabbaabbaba
    aabbabbbaaabaabbabaabbbbbbbbbaab
    baaaabbaaaababaaabbaabbb
    aababaabbaaabbaaaaaabaababaabbbbbbbbbaaa
    bbaabaabbabaaabbbababbababbbabbaaabaabbaabbaaaaaabbbaababbbbabaabbaaaaabbababbaa
    aababaabaababaababbaabab
    bbaaababbbbaaaaaabbaabababaaababbbbaabaa
    abbaabbaabbaabbababbabab
    babbabaabababaabaababbaaabaabaabaaabaabbabbababbaaabbbbaabbaaaaaaababaab
    ababaaababbbbbbaaababbbb
    bbbbbbbababaabaaabbabaab
    baaabaabbabbbbaaaaaaabbb
    ababaabbaaabbabbaaaaabab
    bbaabbbaaaaabbabaabbbaba
    abbaaabbbbbbbabbabaabbaababbbbbbbabbababbababbbb
    baaaabbaabbaaaabaabbabababaaaaaaabbaabaa
    abbabbbaaaabbaaaabababab
    aabaababababaabbbabbabaa
    abaaaabbbaabaaabaaabbbaa
    aabbabbbbbbaaabbaaaabbab
    ababaabbaabbaaaabbaaaaab
    abbbbbaabbbbbbbaaabbaabb
    abaabbbaaaabbbbbabaabbab
    babaaabbbabbbbaabaaaaabbbbbabbaabaabbaaa
    babaabbbbaababaaaabbaaab
    aaabaaaaaabbbababbbaabbabbabbaab
    aabbabaaababbbaabbaaabab
    abbaaaabbabababbbbbbbbbb
    baaabbaaababaaaaababaabbbbabbbaabbbaababbabbbbabbaaaabbaaabababbabaaabbababaaaabbaababbb
    ababaaabbbbabbbbbaabbabaaaaaabaa
    aaababbaabbababbaababaaabaababbabbbaabaababbabba
    baabbbbbabbbbbbaabaabbbbbaaaabbaaabbaabaaabaaabaabaabbab
    aabbbbbaaabababbbbbaaabaaababbaabababaabbaabababbaabbabbaabaababbbabbaabaabbaaba
    baabaaaaaaaabaababbaabbb
    aabaabbbbabbaabbbaababbb
    abbaaabaaabbabbaaaaaaabbaaaaabab
    baabbbbaaaabaaabaaababaaaabbaabb
    abbabbbaabababaaababbbba
    aaaabbbbbbaabbabaabbaabb
    baabaaababbbaabaabbbbbaa
    abbaaabaaabaababbbabaaaa
    bbababaaaabaaaabbabbbbbbababbbbbbbbbaabaabbbbaba
    aabbbbaaaaaaaababaaababa
    bbbabbabaabaaaabbababaabbbaabaabbbaaaaaaaabbbaabbbbabaab
    babababaabbbababbbbbbaaababaaaababbaaabababbaaaaaababbbabbbabaabbaaaababaaaaabbb
    aaababbaaabaaabababbaabbbbabababababaaabbbababba
    bbaabbabababaababaaaaaaa
    bbbaababbabbbbbabbbbaaaa
    baabbaababbabbbabbaaabbbbbbbaabbbaabaaaabbaabbbb
    ababbbababaabbaabababbab
    aaabbaaaabbaaabbabbaabab
    bbaabaaaabaabbbbbbbaabaabbbbbaaabababbbaabbabbbbaababbab
    babbbabababbbababbbabbba
    abbbbabbbbababaaabaabaab
    abababbbabbbabaabbbbababababbabaaaabaaaaaaaaaababbbbbaabbaaabbbb
    babaabbaaaaabbbbabbbbaab
    baabaabbbabaaaabaabbababbbaaabaa
    baaaaaababaaaaaabbaabbabaabbaaaabaaaababbaabaaba
    aabbaaaaabaabbbbabaaaaab
    aaababbbabaababbabbbaabbbababaaaaaabbabbbbabbabaabababbbbbbababbabbbbaabbabababb")

(defn toInt [num] (Integer/parseInt num))

(defn convertRhs [rhs]
    (let [splt (map #( clojure.string/split (clojure.string/trim %1) #"\ ") (-> rhs clojure.string/trim (#(clojure.string/split %1 #"\|"))))
          toInt (fn [strInt] (map #(toInt %1) strInt))]
        (map toInt splt)))

(defn parseRules [rules]
    (let [cleaned (map clojure.string/trim (clojure.string/split-lines rules))
          splt (map #(clojure.string/split % #":") cleaned)
          intg (map #(list (Integer/parseInt (first %)) (second %)) (map #(clojure.string/split % #":") cleaned))
          general (filter #(not (= nil (re-find #"\d+" (second %)))) intg)
          concrete (filter #(= nil (re-find #"\d+" (second %))) intg)
          convertedRhs (reduce #(assoc %1 (first %2) (convertRhs (second %2))) {} general)
          convertedConcrete (reduce #(assoc %1 (first %2) (clojure.string/trim(clojure.string/replace (second %2) #"\"" ""))) {} concrete)]
        (list convertedRhs convertedConcrete)))

(defn parse [input]
    (let [splt (clojure.string/split input #"\n\ +\n")]
        (list (parseRules (first splt)) (map clojure.string/trim (clojure.string/split-lines (second splt))))))

(defn stringToRules [parsed]
    (let [cnvConc (reduce #(assoc %1 (second %2) (first %2)) {} (second (first parsed)))
          convertString (fn [in] (map #(get cnvConc (str %1)) in))]
        (map #(convertString %1) (second parsed))))

(defn convertParsed [parsed]
    (list (first parsed) (stringToRules parsed)))

;https://stackoverflow.com/questions/25690539/function-for-replacing-subsequences
(defn sq-replace
        [match replacement sq]
        (let [matching (count match)]
          ((fn replace-in-sequence [[elt & elts :as sq]]
             (lazy-seq
              (cond (empty? sq)
                    ()
                    (= match (take matching sq))
                    (concat replacement (drop matching sq))
                    :default
                    (cons elt (replace-in-sequence elts)))))
           sq)))

(defn isSubList [in rule]
    (not (empty? (some #{rule} (partition (count rule) 1 in)))))
    ;(= (take (count rule) in) rule))

(defn possibleRules [rules input]
    (let [
        generalRules (first rules)
        matchGivenRule (fn [rule] (some #{true} (map #(isSubList input %1) rule)))
        ;res (filter #(matchGivenRule (second %1)) generalRules)]
        res (filter #(isSubList input (second %1)) generalRules)]
        res))

(defn applyRule [rule input]
    (let [rhs (second rule)
          lhs (first rule)]
          ;possibleRhs (first (filter #(isSubList input %1) rhs))]
          (sq-replace rhs (list lhs) input)))
          ;(cons lhs (drop (count possibleRhs) input))))

(defn tryEachRule [rules input]
    (let [
          possibles (possibleRules rules input)
          generateRule (fn [rule] (map #(vector (first rule) %) (second rule)))
          ;possibles (reduce #(concat %1 %2) (list ) (map #(generateRule %) possiblesRaw))
          containZero (not (empty? (filter #(= 0 (first %)) possibles)))]
        (loop [rems possibles]
            (cond 
                containZero true
                (empty? rems) false
                (tryEachRule rules (applyRule (first rems) input)) true
                :else (recur (rest rems))))))

(defn matchZero [rules input]
    true)

(defn generateRules [rules]
    (let [generateRule (fn [rule] (map #(vector (first rule) %) (second rule)))]
    (list (reduce #(concat %1 %2) (list ) (map #(generateRule %) (first rules))) (second rules))))

(defn solve [parsed]
    (filter #(tryEachRule (generateRules (first parsed)) %1) (second parsed)))

(defn main [input]
    (->> input
        parse
        convertParsed
        solve))