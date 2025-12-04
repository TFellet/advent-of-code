import numpy as np
from scipy.ndimage import convolve

data = open("2025/inputs/day4.txt").read().splitlines()
dn = np.array([[c == "@" for c in row] for row in data]).astype(int)
kernel = np.ones((3,3))
tbr = (convolve(dn, kernel, mode='constant') < 5) & dn
tbr.sum()

dn2 = dn - tbr
while np.any(tbr):
    tbr = (convolve(dn2, kernel, mode='constant') < 5) & dn2
    dn2 -= tbr
dn.sum() - dn2.sum()
