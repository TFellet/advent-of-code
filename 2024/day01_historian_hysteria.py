import polars as pl

a = pl.read_csv("2024/inputs/day1.txt", has_header=False, separator=" ", columns=[0, 3], new_columns=["c1", "c2"])
(a["c1"].sort() - a["c2"].sort()).abs().sum()  # 1879048
g = a.group_by(pl.col("c2").alias("c1_bis")).agg(pl.col("c1").count().alias("count"))
j1, _, j2 = a.join(g, left_on="c1", right_on="c1_bis").get_columns()
(j1 * j2).sum()  # 21024792

# ((a.select(pl.col("c1")).sort(by="c1") - a.select(pl.col("c2")).sort(by="c2")).select(pl.col("c1").abs().sum()).item(0, 0))
# a.join(g, left_on="c1", right_on="c1_bis").select(pl.col("c1") * pl.col("count")).sum().item(0, 0)

# 313 μs


# Numpy version
# import numpy as np
# with open("2024/inputs/day1.txt", "r") as f:
#     data = f.read()
# nums = [int(x) for x in data.split()]
# c1 = np.array(nums[::2])
# c1.sort()
# c2 = np.array(nums[1::2])
# c2.sort()
# np.abs(c1-c2).sum()
# groups2 = Counter(nums[1::2])
# sum([x * groups2[x] for x in c1])
# 434 μs


# Pure Python version
# from collections import Counter
# data = open("2024/inputs/day1.txt", "r").read()
# nums = [int(x) for x in data.split()]
# c1 = nums[::2]
# c1.sort()
# c2 = nums[1::2]
# c2.sort()
# sum([abs(a-b) for a, b in zip(c1, c2)])
# groups2 = Counter(c2)
# sum([x * groups2[x] for x in c1])
# # 464 μs

# from numpy import loadtxt, sort, isin, c_
# import numpy as np

# A, B = sort(loadtxt("2024/inputs/day1.txt", int).T)
# sum(abs(A - B))
# # (((B[None, :] == A[:, None]) * B).sum())
# (np.sum((A==c_[B])*A))

# data = [*map(int, open("2024/inputs/day1.txt").read().split())]
# A, B = sorted(data[0::2]), sorted(data[1::2])
# (sum(map(lambda a, b: abs(a-b), A, B)),
#       sum(map(lambda a: a * B.count(a), A)))
# a = A[0]
# B.count(a)
