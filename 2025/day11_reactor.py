import numpy as np

# Approach 1: Use a transition matrix to count all possible paths
G = {k[:-1]: v for k, *v in map(str.split, open("2025/inputs/day11.txt"))} | {"out": []}
ids = {e: i for i, e in enumerate(set(G.keys()))}

# Create a transition matrix to link data from source to dest
# Rows represent destination, columns represent source
mat = np.array(np.zeros((len(ids), len(ids))))
for s, d in G.items():
    for dx in d:
        mat[ids[dx], ids[s]] = 1


# 1.1.1: Count paths from start to dest in n steps (n is 612 here). Slow because the matrix is computed multiple times,
# and because there is no early stopping
def count_paths(mat, ids, start_id, dest_id):
    m = mat.copy()
    start_id, dest_id = ids[start_id], ids[dest_id]
    m[:, dest_id] = 0  # No exit after reaching destination
    m[dest_id, dest_id] = 1  # Once at destination, stay there
    # Take the transition matrix after n steps and find how many paths lead to destination
    return int(np.linalg.matrix_power(m, len(ids))[dest_id, start_id])


# 1.1.2: Same as 1.1.1 with early stopping
def count_paths1(mat, ids, start_id, dest_id):
    m = mat.copy()
    start_id, dest_id = ids[start_id], ids[dest_id]
    m[:, dest_id] = 0  # No exit after reaching destination
    m[dest_id, dest_id] = 1  # Once at destination, stay there
    # active_ids = np.delete(ids.values(), dest_ids)
    # Take the transition matrix after n steps and find how many paths lead to destination
    while np.any(m[-dest_id, :]):  # While there are some elements outside the destinations ids
        m = m @ m  # Compute the square of the matrix
    # Number of paths from all starts points to all dummy destinations
    return int(m[dest_id, start_id])


# 1.2: Count all paths from all starts to all ends at once, by computing 1 step at a time
# and adding elements seen in destinations. Slow because only 1 step is computed at a time
def count_paths2(mat, starts, ends):
    m = mat.copy()
    res = np.zeros(len(starts))
    while np.any(m):
        res += m[ends, starts].ravel()  # Add number of paths that reached destination in this step
        m = m @ mat  # Compute the next step
    return res


# 1.3: Count all paths from all starts to all ends at once, by computing x² steps at a time
# By computing multiple steps at a time, we can miss when a path reach a point after x+1 steps.
# To avoid this, the destinations needs to loop on themselves
# But looping at a point B will create duplicate paths for a path that goes from A to C using B.
# So we add "dummy" destinations points, where paths go but never escape
# Example: If B was a destination, there will now be a point B' that mimics B, except B' loops on itself and has no exit
# Paths will go to B and B' at the same step. Paths on B' will accumulate, paths on B will continue normally
def count_paths3(mat, starts, ends):
    m = mat.copy()
    dim0 = m.shape[0]
    m = np.vstack([m, m[ends]])  # Extend the matrix in one dimension, with destinations duplicated
    m = np.hstack([m, m[:, ends]])  # Extend the matrix in the other dimension
    ends = [*range(dim0, m.shape[0])]  # New destinations are the last rows/columns
    m[:, ends] = 0  # No exit after reaching dest
    m[ends, ends] = 1  # Once at destination, stay there
    while np.any(m[0:dim0, 0:dim0]):  # While there are still some elements outside the destinations ids
        m = m @ m  # Compute the square of the matrix
    # Number of paths from all starts points to all dummy destinations
    return m[ends, starts].ravel()


starts = [ids[x] for x in ["you", "svr", "fft", "dac", "svr", "dac", "fft"]]
ends = [ids[x] for x in ["out", "fft", "dac", "out", "dac", "fft", "out"]]
res = count_paths2(mat, starts, ends)  # 1.2: 212ms
res = count_paths3(mat, starts, ends)  # 1.3: 30ms
int(res[0])
int(res[1:4].prod()) + int(res[4:].prod())

# 1.1.1: 237 ms
# 1.1.2: 102 ms
count_paths(mat, ids, "you", "out")  # 688
count_paths1(mat, ids, "you", "out")  # 688
if dac_fft := count_paths1(mat, ids, "fft", "dac"):
    p2 = count_paths1(mat, ids, "svr", "fft") * dac_fft * count_paths1(mat, ids, "dac", "out")
else:
    p2 = (
        count_paths1(mat, ids, "svr", "dac")
        * count_paths1(mat, ids, "dac", "fft")
        * count_paths1(mat, ids, "fft", "out")
    )
p2


# Approach 2 (from Reddit): Use graph traversal with memoization

from functools import cache

G = {k[:-1]: v for k, *v in map(str.split, open("2025/inputs/day11.txt"))} | {"out": []}


# Use functool memoization
@cache
def count1(here, dest):
    return here == dest or sum(count1(next, dest) for next in G[here])


def count2(here, dest):
    if here == dest:
        return 1
    if (here, dest) in dcache:
        return dcache[(here, dest)]
    res = 0
    for next in G[here]:
        res += count2(next, dest)
    dcache[(here, dest)] = res
    return res


# Custom memoization for count2
dcache = {}
count = count1
count("you", "out")
# Only 1 solution is valid to avoid cycles, dag to fft or fft to dag
count("svr", "dac") * dac_fft * count("fft", "out") if (dac_fft := count("dac", "fft")) else count(
    "svr", "fft"
) * count("fft", "dac") * count("dac", "out")


# Approach 3: Set theory (Also from Reddit)
# [All paths] + [All paths without any objectives] - [All paths without objective 1] -
#   [All paths without objective 2] = [All paths with both objectives].
def count3_helper(dcache2, filter):
    dcache2 = {}
    dcache2["out"] = 1
    for k in filter:
        dcache2[k] = 0

    return count3("svr", dcache2)


def count3(here, dcache2):
    if here in dcache2:
        return dcache2[here]
    res = 0
    for next in G[here]:
        res += count3(next, dcache2)
    dcache2[here] = res
    return res


nodac = count3_helper("svr", ["dac"])
nofft = count3_helper("svr", ["fft"])
nodacorfft = count3_helper("svr", ["dac", "fft"])
all = count3_helper("svr", [])
all + nodacorfft - nofft - nodac
# 293263494406608
