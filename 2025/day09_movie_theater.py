import numpy as np

data = open("2025/inputs/day9.txt").readlines()
data = [tuple(map(int, r.split(","))) for r in data]
# "Compress" the coordinates by mapping each unique x and y coordinate to an integer index
mapx = {k: v for v, k in enumerate(sorted(set([r[0] for r in data])))}
mapy = {k: v for v, k in enumerate(sorted(set([r[1] for r in data])))}
revertx = {v: k for k, v in mapx.items()}
reverty = {v: k for k, v in mapy.items()}
data2 = [(mapx[x], mapy[y]) for x, y in data]
data = data2

max_area = 0
for i in range(len(data)):
    for j in range(i + 1, len(data)):
        area = abs((revertx[data[i][0]] - revertx[data[j][0]]) + 1) * abs(
            (reverty[data[i][1]] - reverty[data[j][1]]) + 1
        )
        if area > max_area:
            max_area = area
max_area  # 4776487744

lx = max(data, key=lambda x: x[0])[0] + 1
ly = max(data, key=lambda x: x[1])[1] + 1
le = max([lx, ly])
maxx = np.zeros(le)  # Maximum value of X for each Y coordinate
maxy = np.zeros(le)  # Maximum value of Y for each X coordinate
minx = np.full(le, lx)  # Minimum value of X for each Y coordinate
miny = np.full(le, ly)  # Minimum value of Y for each X coordinate
# Remember the contour of the shape S in the 4 arrays
for i in range(len(data)):
    x1, y1 = data[i]
    x2, y2 = data[(i + 1) % len(data)]
    if x1 == x2:  # Vertical line
        if y1 > y2:
            y1, y2 = y2, y1
        y2 += 1
        minx[y1:y2] = np.minimum(minx[y1:y2], x1)
        maxx[y1:y2] = np.maximum(maxx[y1:y2], x1)
    elif y1 == y2:  # Horizontal line
        if x1 > x2:
            x1, x2 = x2, x1
        x2 += 1
        miny[x1:x2] = np.minimum(miny[x1:x2], y1)
        maxy[x1:x2] = np.maximum(maxy[x1:x2], y1)

# Find the largest rectangle that fits inside the shape S
max_area2 = 0
for i in range(len(data)):
    for j in range(i + 1, len(data)):
        x1, y1 = data[i]
        x2, y2 = data[j]
        if x1 > x2:
            x1, x2 = x2, x1
        if y1 > y2:
            y1, y2 = y2, y1
        x2 += 1
        y2 += 1
        if ( # Check if the contour of the rectangle is completely inside the shape S
            np.all(minx[y1:y2] <= x1)
            and np.all(maxx[y1:y2] >= x2)
            and np.all(miny[x1:x2] <= y1)
            and np.all(maxx[y1:y2] >= x2)
        ):
            area = (revertx[x2 - 1] - revertx[x1] + 1) * (reverty[y2 - 1] - reverty[y1] + 1)
            if area > max_area2:
                max_area2 = area
max_area2
# 1560299548
# 1560299548
