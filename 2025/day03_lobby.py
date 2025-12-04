data = open("2025/inputs/day3.txt").read().strip().split()
def maxn(s: str, n: int):
    res = ""
    st = 0
    for i in range(n):
        en = (len(s) - n + i + 1)
        max_i = max(s[st:en])
        st = s.index(max_i, st, en) + 1
        res += max_i
    return int(res)
sum(maxn(r, 2) for r in data)
sum(maxn(r, 12) for r in data)
