from functools import cmp_to_key

a = open("2024/inputs/day5.txt", "r").read().split("\n\n")

rules = set([(int(y[0]), int(y[1])) for x in a[0].splitlines() if (y := x.split("|"))])
updates = [[int(y) for y in x.split(",")] for x in a[1].splitlines()]

# Comparison function: If a tuple is present in the set, return -1 to sort the first value before the second
key = cmp_to_key(lambda a, b: -((a, b) in rules))

count = [0, 0]
for up in updates:
    s = sorted(up, key=key)
    count[up != s] += s[len(s) // 2]  # Update ounter depending on whether the original list was already sorted

print(count)

# def parseInts(txt, sep):
# return list(map(lambda x: [int(y) for y in x.split(sep)], txt))

# updates = list(map(lambda x: [int(y) for y in x.split(',')], a[a.index('')+1:]))
# inc = updates.copy()
# first = True
# count = [0, 0]
# while len(inc) > 0:
#     rm = []
#     for up in inc:
#         valid = True
#         for rule in rules:
#             rule1 = up.index(rule[0]) if rule[0] in up else -1
#             rule2 = up.index(rule[1]) if rule[1] in up else 999
#             if rule2 < rule1:
#                 valid = False
#                 up[rule1], up[rule2] = up[rule2], up[rule1]
#         if valid:
#             count[not first] += up[len(up)//2]
#     [inc.remove(x) for x in rm]
#     first = False
# print(*count) # 5747, 5502
