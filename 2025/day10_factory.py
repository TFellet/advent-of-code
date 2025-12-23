from functools import cache, reduce
from itertools import combinations

from scipy.optimize import linprog

# Approach 1: Part 1 with bitmask and part 2 with a linear solver (191 ms)

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

def press(buttons_bit, goal) -> int:
    for n in range(1, len(buttons_bit) + 1):
        for pressed in combinations(buttons_bit, n):
            if reduce(lambda a, b: a ^ b, pressed or [0]) == goal:
                return n
    return -1

p1 = p2 = 0
data_s = [d.strip().split(" ") for d in open("2025/inputs/day10.txt")]

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
p1, int(p2)  # (514, 21824)


# Approach 2: Recursive solution (143 ms (Numba) / 195 ms (Pure Python))
# (https://www.reddit.com/r/adventofcode/comments/1pk87hl/2025_day_10_part_2_bifurcate_your_way_to_victory/)

# We simulate all buttons presses combinations. For each combination we save the end state, the parity of the
# end state, and the number of buttons pressed.
# The result is a dictionnary of possible parity states, and for each parity state we have a dictionnary of
# end state and number of buttons pressed
# Ex: {(0,1,1,1): (0,1,1,1): 2, (0,3,1,3): 4}

# To solve part 1, we look at parity of the solution, we find it in our dictionnary of combinations, and we
# return the minimum value found for buttons pressed.

# For part 2 we need to find the minimum number of button presses to reach the target state.
# Since the target state have a specific parity, every path to reach the target state must always include a specific
# combination of buttons that result in the target parity.
# To solve part 2, we look at the parity of the solution and we find it in our dictionnary of combinations.
# To explore a path, we substract the state found in our dictionnary from the target state (to get a current state).
# This gives us a new state where every number is even.
# The solution at this point will consist of a combinations of buttons, each pressed twice
# To find this combination of buttons, we divide the current state by 2.
# This results in a new current state, with a new parity pattern.
# We can apply the same process as before, until our current state is all zeros.
# Let's define npX as the number of button pressed to get a parity pattern at step X.
# The total number of button pressed is (np1 + 2*(np2 + 2*(np3 + 2*(...))))
# We explore each possible path at each step (here using DFS but BFS would work as well) and return the minimum.

# Simulate every combination of button presses.
# For each combination, store the raw resulting pattern, the parity pattern, and the number of button pressed
# Group the results by parity pattern

numba = False
if numba:
    # Use numba to simulate all combination of button presses. Faster than pure python but harder to read
    from numba import njit

    @njit()
    def state_list(buttons_bin):
        state0 = [0] * len(buttons_bin[0])
        # The total number of combinations is 2^(number of buttons), because each button can be pressed or not
        ncomb = 2 ** len(buttons_bin)
        state_list = [state0] * ncomb
        parity_list = [state0] * ncomb
        npressed = [0] * ncomb
        # Interpret the combination number as binary (ex for 3 buttons ABC: 6 => 110 => A and B are pressed, C is not)
        for i in range(ncomb):
            state = state0.copy()
            n = 0
            for j in range(len(buttons_bin)):
                # If button at index j is pressed (bit j of i is 1)
                if (i >> j) & 1:
                    n += 1
                    # Add button j to state
                    for x in range(len(buttons_bin[j])):
                        state[x] += buttons_bin[j][x]
            parity = [x % 2 for x in state]
            state_list[i] = state
            parity_list[i] = parity
            npressed[i] = n
        return parity_list, state_list, npressed

    def parity_pattern(buttons_bin):
        res = state_list(buttons_bin)
        pattern_dict = {}
        for parity, pattern, cost in zip(res[0], res[1], res[2]):
            parity = tuple(parity)
            pattern = tuple(pattern)
            if parity not in pattern_dict:
                pattern_dict[parity] = {}
                pattern_dict[parity][pattern] = cost
            elif pattern not in pattern_dict[parity]:
                pattern_dict[parity][pattern] = cost
            else:
                # Combination are not explored in order of number of button presses
                # So a state might be reachable another way with less buttons
                pattern_dict[parity][pattern] = min(pattern_dict[parity][pattern], cost)
        return pattern_dict

else:
    from typing import Dict, List

    def parity_pattern(buttons_bin: List[tuple[int]]) -> Dict[tuple, Dict[tuple, int]]:
        patterns = {}
        # Add the 0 presses pattern: If after dividing by 2, the state still consists of even values, a valid strategy
        # is to not press any button and divide by 2 again
        rep0 = tuple([0] * len(buttons_bin[0]))
        patterns[rep0] = {rep0: 0}
        for n in range(1, len(buttons_bin) + 1): # Press n buttons
            for pressed in combinations(buttons_bin, n): # Each combination of n buttons pressed
                res = tuple(map(sum, zip(*pressed))) # Resulting state after pressing these buttons
                parity = tuple(r % 2 for r in res)
                if parity not in patterns: # First encounter of the parity
                    patterns[parity] = {} # Create the dict for this parity
                    patterns[parity][res] = n # Add the state with its cost
                elif res not in patterns[parity]: # Parity is present but state isn't
                    patterns[parity][res] = n # Add the state with its cost
        return patterns


def solve(jolt, patterns_dict):
    @cache # Multiple paths can lead to the same state, so we cache the results
    def solve_iter(jolt):
        if all(j == 0 for j in jolt): # Reached the goal
            return 0
        jolt_parity = tuple(r % 2 for r in jolt) # Current parity to reach
        min_cost = 1000000
        if jolt_parity not in patterns_dict: # Parity pattern not achievable by button presses, return very high value
            return min_cost
        for pattern, cost in patterns_dict[jolt_parity].items(): # Try every button combination that reaches the parity
            if any(i > j for i, j in zip(pattern, jolt)): # If any button will be pressed too many times, try next path
                continue
            new_jolt = tuple((j - p) // 2 for j, p in zip(jolt, pattern)) # New state after pressing buttons
            new_cost = 2 * solve_iter(new_jolt) + cost
            if new_cost < min_cost: # Update minimum cost if better path found
                min_cost = new_cost
        return min_cost

    return solve_iter(jolt)

data_s = [d.strip().split(" ") for d in open("2025/inputs/day10.txt")]

p1 = p2 = 0
for goal, *buttons, jolt in data_s:
    goal = [int(c == "#") for c in goal[1:-1]]
    buttons = [tuple(map(int, x[1:-1].split(","))) for x in buttons]
    buttons_bin = []
    for button in buttons:
        z = [0] * len(goal)
        for b in button:
            z[b] = 1
        buttons_bin.append(tuple(z))
    patterns_dict = parity_pattern(buttons_bin)
    p1 += min(patterns_dict[tuple(goal)].values())
    jolt = tuple(map(int, jolt[1:-1].split(",")))
    p2 += solve(jolt, patterns_dict)
p1, p2
