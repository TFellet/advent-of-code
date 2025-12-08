import math

data = open("2025/inputs/day6.txt").read().splitlines()
data2 = [filter(None, r.split(' ')) for r in data]
data3 = list(map(list, zip(*data2)))
p1 = 0
for r in data3:
    ri = list(map(int, r[:-1]))
    op = sum if r[-1] == '+' else math.prod
    p1 += op(ri)
p1

p2 = 0
nums = []
for i in range(len(data[0])-1, -1, -1):
    s = "".join([j[i] for j in data[:-1]]).rstrip()
    if s:
        nums.append(int(s))
    if (oper := data[-1][i]) != ' ':
        op = sum if oper == '+' else math.prod
        p2 += op(nums)
        nums.clear()
p2
