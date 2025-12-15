from functools import reduce
from itertools import combinations

from scipy.optimize import linprog

p1 = p2 = 0
data_s = [d.strip().split(" ") for d in open("2025/inputs/day10.txt")]


def press(buttons_bit, goal) -> int:
    for n in range(1, len(buttons_bit) + 1):
        for pressed in combinations(buttons_bit, n):
            if reduce(lambda a, b: a ^ b, pressed or [0]) == goal:
                return n
    return -1


for goal, *buttons, jolt in data_s:
    # Goal to reach in part 1
    goal = [int(c == "#") for c in goal[1:-1]]
    # Convert goal to bit representation. Ex: [1, 0, 1] -> 5
    goal_bit = sum(i * 2**n for n, i in enumerate(goal))
    # List of buttons. A button switch multiple bits
    buttons = [list(map(int, x[1:-1].split(","))) for x in buttons]
    # Convert buttons to bit representation. Ex: [1, 8] -> 258
    buttons_bit = [sum(2**b for b in button) for button in buttons]
    p1 += press(buttons_bit, goal_bit)

    jolt = tuple(map(int, jolt[1:-1].split(",")))
    numbers = range(len(jolt))
    lhs = [[n in b for b in buttons] for n in numbers]
    p2 += linprog([1] * len(buttons), A_eq=lhs, b_eq=jolt, integrality=1).fun
p1, int(p2)  # (519, 21824)

# For part 2, the objective is to find the minimum number of button presses to reach the goal:
# '(3)', '(1,3)', '(2)', '(2,3)', '(0,2)', '(0,1)' => '{3,5,4,7}'
# Buttons are numbered from 0 to 5.
# Target 0 is affected by buttons 4 and 5 ((0,2) and (0,1))
# Target 1 is affected by buttons 1 and 5 ((1,3) and (0,1))
# repeat ...
# There are 6 coefficients a to f for the buttons 0 to 5
# We model it as an equation of this form:
# e * (0,2) + f * (0,1) = 3
# b * (1,3) + f * (0,1) = 5
# repeat ...
# LHS is a matrix of size 6x4 (6 buttons, 4 targets)
# We use linprog to solve this linear system of equations by using A_eq and B_eq
# The first argument c is the coefficients to minimize (here all 1's, all coefficients are equally importants)
# Integrality=1 limits solutions to integer values
# fun is the minimum value of the objective function