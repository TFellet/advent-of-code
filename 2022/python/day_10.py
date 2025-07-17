import numpy as np

f = lambda x: int(x) if x[-1].isdigit() else 0
X = np.array([1, *map(f, open('../inputs/day10.txt').read().split()[:-1])]).cumsum()
idx = np.arange(6)*40 + 20
(X[idx-1]*idx).sum() # Part 1 (13860): Sum of all numbers at specified indexes
draw = (abs((np.arange(len(X)))%40 - (X)) < 2).reshape(6, 40) # Part 2 (RZHFGJCB): Visible letters in the matrix
# from matplotlib import pyplot as plt
# plt.imshow(draw, cmap='binary')
