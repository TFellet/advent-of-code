from collections import defaultdict

data = open("2025/inputs/day7.txt").readlines()
p1 = 0
beams = {data[0].index("S"): 1}
for r in data[1:]:
    row_beams = defaultdict(int)
    for beam in beams:
        if r[beam] == "^":
            p1 += 1
            row_beams[beam - 1] += beams[beam]
            row_beams[beam + 1] += beams[beam]
        else:
            row_beams[beam] += beams[beam]
    beams = row_beams
p1, sum(beams.values())
