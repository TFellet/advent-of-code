from multiprocessing import Pool, set_start_method

set_start_method("fork")

##### Using tuple and 2D array (faster) #####

a = open("2024/inputs/day6.txt", "r").read().splitlines()
R = len(a)
C = len(a[0])
guard = (0, 0)
dirs = [(-1, 0), (0, 1), (1, 0), (0, -1)]

grid = []

for r in range(R):
    line = [0] * C
    for c in range(C):
        letter = a[r][c]
        if letter == "^":
            guard = (r + 1, c + 1)
        elif letter == "#":
            line[c] = -1
    grid.append(line)


def pad(grid, p=0):
    for row in grid:
        row.insert(0, p)
        row.append(p)
    grid.insert(0, [p] * len(grid[0]))
    grid.append([p] * len(grid[0]))
    return grid


grid = pad(grid, 0)
grid[guard[0]][guard[1]] |= 2**0


def sim2(grid, guard, obstacle=None, dir=0, p1=False):
    grid = [li.copy() for li in grid]
    if obstacle:
        grid[obstacle[0]][obstacle[1]] = -1
    while True:
        next_pos = (guard[0] + dirs[dir][0], guard[1] + dirs[dir][1])
        val = grid[next_pos[0]][next_pos[1]]
        if val == -1:
            dir = (dir + 1) % 4
        else:
            guard = next_pos
            if val & (2**dir):  # If position already visited in this direction
                return True
            # Guard off grid
            if not (0 < guard[0] < (C + 1)) or not (0 < guard[1] < (R + 1)):
                return grid if p1 else False
            # Store position taken as bitmask
            grid[guard[0]][guard[1]] |= 2**dir


grid_walked: list[list[int]] = sim2(grid, guard, p1=True)  # type: ignore
unique_pos = []
for r in range(len(grid_walked)):
    for c in range(len(grid_walked[0])):
        if grid_walked[r][c] > 0:
            unique_pos.append((r, c))
print(len(unique_pos))  # 5131

with Pool(12) as p:
    results = sum(p.starmap(sim2, ((grid, guard, obs) for obs in unique_pos)))  # type: ignore
print(results)  # 1784

# sum((sim2(grid, guard, obstacle=obs) for obs in unique_pos)) #type: ignore

##### Using complex numbers and sets (slower) #####
# a = open("2024/inputs/day6.txt", "r").read().splitlines()
# R = len(a)
# C = len(a[0])

# guard = 0
# dir = 0
# d = {"^": 1j, ">": 1, "v": -1j, "<": -1}
# crates = set()

# for r in range(R):
#     for c in range(C):
#         pos = (R - r) * 1j + c
#         letter = a[r][c]
#         if letter in d.keys():
#             guard = pos
#             dir = d[letter]
#         elif letter == "#":
#             crates.add(pos)


# def simulation(guard, dir, crates, obstacle=None, p1=False):
#     if obstacle:
#         crates.add(obstacle)
#     taken = set()
#     while True:
#         next_pos = guard + dir
#         if next_pos in crates:
#             dir *= -1j
#         else:
#             guard = next_pos
#             if (guard, dir) in taken:
#                 return True
#             if not (0 <= guard.imag < C) or not (0 <= guard.real < R):
#                 if p1:
#                     return taken
#                 else:
#                     return False
#             taken.add((guard, dir))


# taken = simulation(guard, dir, crates, p1=True)
# unique_pos = set([x[0] for x in taken])  # type: ignore
# print(len(unique_pos))

# with Pool(12) as p:
#     results = sum(p.starmap(simulation, ((guard, dir, crates.copy(), pos) for pos in unique_pos)))  # type: ignore
# print(results)
# sum([simulation(guard, dir, crates.copy(), pos) for pos in unique_pos])  # type: ignore
