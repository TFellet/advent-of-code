import numpy as np

data = open("2025/inputs/day11.txt").readlines()
source = [d[0:3] for d in data]
dest = [d[5:].strip().split(" ") for d in data]
elements = set(source)
elements.add("out")
ids = {e: i for i, e in enumerate(elements)}

# Create a transition matrix to link data from source to dest
# Rows represent destination, columns represent source
mat = np.matrix(np.zeros((len(elements), len(elements))))
for s, d in zip(source, dest):
    for dx in d:
        mat[ids[dx], ids[s]] = 1

def count_paths(mat, ids, start_id, dest_id):
    start = np.zeros(len(ids))
    start[ids[start_id]] = 1
    m = mat.copy()
    m[:, ids[dest_id]] = 0
    npaths = 0
    for i in range(len(ids)):
        m = m.__matmul__(mat)
        if not np.any(m):
            break
        res = m.__matmul__(start)[:, ids[dest_id]].sum()
        npaths += res
    return int(npaths)

count_paths(mat, ids, "you", "out")  # 688
(
    count_paths(mat, ids, "svr", "fft") * count_paths(mat, ids, "fft", "dac") * count_paths(mat, ids, "dac", "out")
    + count_paths(mat, ids, "svr", "dac") * count_paths(mat, ids, "dac", "fft") * count_paths(mat, ids, "fft", "out")
)
