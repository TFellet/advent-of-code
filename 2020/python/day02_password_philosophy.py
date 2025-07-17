import re
import numpy as np
with open('2020/inputs/day2.txt','r') as fi: a = fi.readlines()

r=re.compile('(\d+)-(\d+) (.): (.*)')
res = np.array([re.match(r, x).groups() for x in a], dtype=np.dtype('int,int,U1,U20'))
counts = np.array([x[3].count(x[2]) for x in res])
np.sum((counts>=res['f0']) & (counts<=res['f1']))

sum(bool((x[3][x[0]-1] == x[2]) ^ (x[3][x[1]-1] == x[2])) for x in res)
