import math
from collections import Counter

data = open("2025/inputs/day8.txt").readlines()
# Read points into int tuples
data_int = [(int(s[0]), int(s[1]), int(s[2])) for r in data if (s := r.split(","))]
le = len(data_int)
# Compute distance between all possible pairs of points and sort them
dist_list = []
for i in range(le):
    for j in range(i + 1, le):
        dist_list.append(((i, j), math.dist(data_int[i], data_int[j])))
sdist = sorted(dist_list, key=lambda x: x[1])

circuits = {i: i for i in range(le)}  # Each point belong to a circuit with its id
p1 = p2 = i = 0
j = len(circuits.values())
for p, _ in sdist:  # p is the pair of points connected
    i += 1
    # Get the circuit id of the pair of points
    c1 = circuits[p[0]]
    c2 = circuits[p[1]]
    if c1 == c2:  # Already in the same circuit, continue to the next connection
        continue
    # Merge the two circuits by changing the id of one to match the other wherever it appears
    for k, v in circuits.items():
        if v == c2:
            circuits[k] = c1
    if i == 1000:  # On connection 1000, get the product of the top 3 largest circuits sizes
        p1 = math.prod(x for _, x in Counter(circuits.values()).most_common(3))
    j -= 1  # Number of distinct circuits left
    if j == 1:  # When there's only one circuit left, multipy the X coordinate of the last 2 points connected
        p2 = data_int[p[0]][0] * data_int[p[1]][0]
        break
p1, p2
