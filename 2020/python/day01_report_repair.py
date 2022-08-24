import itertools

with open('../inputs/day1.txt','r') as fi: a2 = list(map(int, fi.readlines()))
goal=2020
ainf, asup = set(), set()
for x in a2: asup.add(goal-x) if x > (goal/2) else ainf.add(x)
v1 = ainf.intersection(asup).pop()

(goal-v1)*v1

for n in itertools.product(ainf, ainf):
  if sum(n) in asup: res=(goal-sum(n))*n[0]*n[1]; break
res
