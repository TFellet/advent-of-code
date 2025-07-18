import re

am = open("2024/inputs/day4.txt", "r").read().splitlines()

w = len(am[0])
h = len(am)
lim = 4
d = []
# Find the text in all diagonals
# Diagonals where y starts at 0
for i in range(w):
    # Negative diagonal
    x = [*range(i, -1, -1)]
    y = [*range(0, len(x))]
    if len(x) >= lim:
        d.append("".join([am[y][x] for x, y in zip(x, y)]))
    # Positive diagonal
    x = [*range(i, w)]
    y = [*range(0, len(x))]
    if len(x) >= lim:
        d.append("".join([am[y][x] for x, y in zip(x, y)]))
# Diagonals where x starts at h for negative diagonal and 0 for positive diagonal
for i in range(1, h):
    y = [*range(i, h)]  # Y always goes down
    x = [*range(h - 1, h - len(y) - 1, -1)]  # Negative diagonal
    x2 = [*range(0, len(y))]  # Positive diagonal
    if len(y) >= lim:
        d.append("".join([am[y][x] for x, y in zip(x, y)]))
        d.append("".join([am[y][x] for x, y in zip(x2, y)]))

# Transpose the text
amt = ["".join(x) for x in list(map(list, zip(*am)))]

# Concat the regular text, the transposed text and the diagonal text in a single string
all_str = "|".join([*am, *amt, *d])
count1 = len(re.findall("XMAS", all_str)) + len(re.findall("SAMX", all_str))
print(count1)
# 2496


pattern = list("MAS")
pattern2 = list("SAM")
count2 = 0
# Search at every point of the grid for a 3x3 pattern
for i in range(w - 2):
    for j in range(h - 2):
        l1 = [am[j + k][i + k] for k in range(3)]  # Positive diagonal
        l2 = [am[j + 2 - k][i + k] for k in range(3)]  # Negative diagonal
        # If both diagonals match either pattern
        if ((l1 == pattern) or l1 == pattern2) and ((l2 == pattern) or l2 == pattern2):
            count2 += 1
print(count2)

# from collections import defaultdict

# with open("2024/inputs/day4.txt", "r") as f:
#     a = f.read()
# G = defaultdict(str) | {(i,j):c for i,r in enumerate(a.splitlines())
#                                 for j,c in enumerate(r)}
# g = list(G.keys())
# D = -1,0,1

# T = list('XMAS'),
# (sum([G[i+di*n, j+dj*n] for n in range(4)] in T
#                 for di in D for dj in D for i,j in g))

# T = list('MAS'), list('SAM')
# (sum([G[i+d, j+d] for d in D] in T
#       and [G[i+d, j-d] for d in D] in T for i,j in g))
