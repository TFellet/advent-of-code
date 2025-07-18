a = open("2024/inputs/day2.txt", "r").readlines()


def test_line(l2):
    asc = 1
    for i in range(len(l2) - 1):
        if i == 0:
            asc = 1 if l2[i] < l2[i + 1] else -1
        d = (l2[i + 1] - l2[i]) * asc
        if (d > 3) | (d < 1):
            return False
    return True


n1 = 0
n2 = 0
for l1 in a:
    l2 = [int(x) for x in l1.split()]
    if test_line(l2):
        n1 += 1
    else:
        for i in range(len(l2)):
            f = [True] * len(l2)
            f[i] = False
            l3 = [l2[j] for j in range(len(l2)) if f[j]]
            if test_line(l3):
                n2 += 1
                break
print(n1)
print(n1 + n2)
# 2.7 ms

# import os
# import polars as pl

# os.environ['POLARS_MAX_THREADS'] = '8'

# # %%
# %%timeit -n 10
# # 124 ms
# a = pl.read_csv("2024/inputs/day2.txt", has_header=False, separator=' ').transpose()
# d = a[1:] - a[:-1]

# # d = a.select(pl.all().diff(null_behavior='drop'))
# exp = (
#         (pl.all() >= 1) & (pl.all() <= 3)
#     ).all() | (
#         (pl.all() <= -1) & (pl.all() >= -3)
#     ).all()
# r = d.select(exp).transpose()

# p1 = r.sum().item(0, 0) # 534

# a2 = a[:,~r[:,0]]
# def test(i):
#     b = [True] * a2.height
#     b[i] = False
#     atmp = a2.filter(b)
#     d = atmp[1:] - atmp[:-1]
#     return d.select(exp)

# a3 = pl.concat([test(i) for i in range(a2.height)])
# (a3.sum().transpose() > 0).sum().item(0,0) + p1 # 577
