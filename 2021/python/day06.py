import numpy as np
from collections import Counter

with open('../inputs/day6.txt', 'r', encoding='utf-8') as f: a = f.readline()
V0 = np.zeros(9, dtype=int)
for k,v in Counter([int(x) for x in a.split(',')]).items(): V0[k] = v
A = np.zeros(81, dtype=int)
for i in np.arange(1,81,10): A[i] = 1
A.shape = (9,9)
A[6,0] = A[8,0] = 1

def nf(f,d): return np.linalg.matrix_power(A, d).__matmul__(f).sum()
nf(V0, 80) # Part 1
nf(V0, 256) # Part 2
