from functools import reduce
from itertools import combinations

from scipy.optimize import linprog

p1 = p2 = 0
data = open("2025/inputs/day10.txt").readlines()
data_s = [d.strip().split(" ") for d in data]


def press(buttons_bit, numbers, goal):
    for n in numbers:
        for pressed in combinations(buttons_bit, n):
            if reduce(lambda a, b: a ^ b, pressed or [0]) == goal:
                return n
    return -1


for goal, *buttons, jolt in data_s:
    goal = [int(c == "#") for c in goal[1:-1]]
    goal_bit = sum(i * 2**n for n, i in enumerate(goal))
    buttons = [list(map(int, x[1:-1].split(","))) for x in buttons]
    buttons_bit = [sum(2**b for b in button) for button in buttons]

    jolt = tuple(map(int, jolt[1:-1].split(",")))
    numbers = range(len(jolt))
    p1 += press(buttons_bit, numbers, goal_bit)
    lhs = [[n in b for b in buttons] for n in numbers]
    p2 += linprog([1] * len(buttons), A_eq=lhs, b_eq=jolt, integrality=1).fun
p1, int(p2)
