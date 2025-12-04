import functools
from math import ceil, floor


# Generate numbers of style 11, 101, 10101, 10001, etc. that gives integer results when dividing a digit with repeats
# Ex: In 6 digits numbers, 111111 checks repeats of length 1, 10101 repeats of length 2, and 1001 repeats of length 3
@functools.cache
def gen_magic(d):
    return [
        int(("1" + "0" * (len_rep - 1)) * (d // len_rep - 1) + "1")
        for len_rep in range(1, d // 2 + 1) # The length of repeat ranges from 1 to half of d
        if (d / len_rep) % 1 == 0 # Only consider lengths that divide evenly into d
    ]


# Check how many digits between s and e are divisible by the magic number
@functools.cache
def check_num(magicnum, s, e):
    return [i * magicnum for i in range(ceil(s / magicnum), floor(e / magicnum) + 1)]

with open("2025/inputs/day2.txt") as f:
    ranges = f.read().strip().split(",")


def get_invalids(rs):
    p1 = [0]
    p2 = set()
    s, e = map(int, rs)
    # If there are a different number of digits inside the range, we need to split it
    if len(rs[0]) < len(rs[1]):
        newval = int("9" * len(rs[0]))
        # Add the results of next split recursively
        res = get_invalids((str(newval + 1), rs[1]))
        p1.extend(res[0])
        p2.update(res[1])
        # Change the end of this split to have the same number of digits as the start
        e = newval
    magics = gen_magic(len(rs[0]))
    if len(rs[0]) % 2 != 1: # For part 1, look only at numbers with an even number of digits
        p1.extend(check_num(magics[-1], s, e))
    for m in magics:
        p2.update(check_num(m, s, e))
    return p1, p2


p1 = []
p2 = set()
for r in ranges:
    res = get_invalids(r.split("-"))
    p1.extend(res[0])
    p2.update(res[1])
sum(p1)  # 19605500130
sum(p2)  # 36862281418
