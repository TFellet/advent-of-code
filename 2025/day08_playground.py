import math
from collections import Counter

from scipy.cluster.hierarchy import DisjointSet

data = open("2025/inputs/day8.txt").readlines()
# Read points into int tuples
data_int = [tuple(map(int, r.split(","))) for r in data]
le = len(data_int)

## Approach 1: Dictionnary implementation

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


## Approach 2: Using scipy's disjoint set implementation

dist_list2 = []
for i in range(le):
    for j in range(i + 1, le):
        dist_list2.append(((data_int[i], data_int[j]), math.dist(data_int[i], data_int[j])))
sdist2 = [x[0] for x in sorted(dist_list2, key=lambda x: x[1])]

p1 = p2 = i = 0
dset = DisjointSet(data_int)
for po1, po2 in sdist2:
    i += 1
    dset.merge(po1, po2)
    if i == 1000:
        p1 = math.prod(sorted((len(s) for s in dset.subsets()), reverse=True)[:3])
    if dset.n_subsets == 1:
        p2 = po1[0] * po2[0]
        break
p1, p2



## Approach 3: Implementing a custom disjoint set implementation (slighty faster than scipy for some reason)


class disSet:
    def __init__(self, vertices):
        self.vertices = vertices
        self.n = len(vertices)  # Number of trees
        self.parent = {i: i for i in vertices}  # Each item is its own parent at the start
        self.ranks = {i: 0 for i in vertices}  # All trees are rank 0

    def __getitem__(self, item):
        """Find the root of the set that contains item, while flattening the tree on the way"""
        if self.parent[item] != item:  # If the item is not at the top of the tree
            self.parent[item] = self[self.parent[item]]  # Update it's parent to be the top of the tree
        return self.parent[item]  # Return the root of the tree

    def union(self, set1, set2):
        """Union two sets together by rank"""
        root1 = self[set1]
        root2 = self[set2]
        if root1 != root2:  # If the 2 items have different roots, they need to be merged
            # Merge smaller ranked tree into larger one
            if self.ranks[root1] < self.ranks[root2]:
                self.parent[root1] = root2
            else:
                self.parent[root2] = root1
                if self.ranks[root1] == self.ranks[root2]:  # If both tree have the same rank, increase rank by 1
                    self.ranks[root1] += 1
            self.n -= 1  # When 2 sets have been merged, there is 1 less set

    def flatten(self):
        """Flatten all trees to get an accurate size of each set when looking at its parent"""
        for i in self.vertices:
            self[i]
        return self.parent


dist_list2 = []
for i in range(le):
    for j in range(i + 1, le):
        dist_list2.append(((data_int[i], data_int[j]), math.dist(data_int[i], data_int[j])))
sdist2 = [x[0] for x in sorted(dist_list2, key=lambda x: x[1])]

dis_set = disSet(data_int)
p1 = p2 = i = 0
for po1, po2 in sdist2:
    i += 1
    dis_set.union(po1, po2)
    if i == 1000:
        p1 = math.prod(x for _, x in Counter(dis_set.flatten().values()).most_common(3))
    if dis_set.n == 1:
        p2 = po1[0] * po2[0]
        break
p1, p2
