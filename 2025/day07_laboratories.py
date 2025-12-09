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

# Alternative solution using a single array instead of a dict
data = open("2025/inputs/day7.txt").readlines()
paths = [0] * len(data[0])
paths[data[0].index("S")] = 1
p1 = 0
for r in data[1:]:
    if (st := r.find("^")) == -1:
        continue
    for i in range(st, len(paths)):
        if r[i] == "^":
            p1 += paths[i] > 0
            paths[i - 1] += paths[i]
            paths[i + 1] += paths[i]
            paths[i] = 0
p1, sum(paths) # (1638, 7759107121385)
