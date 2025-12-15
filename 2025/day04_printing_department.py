import numpy as np
from scipy.ndimage import convolve

dn = np.array([[c == "@" for c in row] for row in open("2025/inputs/day4.txt")]).astype(int)
kernel = np.ones((3,3))
tbr = (convolve(dn, kernel, mode='constant') < 5) & dn
tbr.sum()

dn2 = dn - tbr
while np.any(tbr):
    tbr = (convolve(dn2, kernel, mode='constant') < 5) & dn2
    dn2 -= tbr
dn.sum() - dn2.sum()
