import numpy as np

data = [tuple(map(int, x.split(','))) for x in open("2025/inputs/day9.txt")]

area_list = []
for i in range(len(data)):
    for j in range(i + 1, len(data)):
        # xi, xj, yi, yj = data[i][0], data[j][0], data[i][1], data[j][1]
        (xi, yi), (xj, yj) = data[i], data[j]
        area = (abs(xi - xj) + 1) * (abs(yi - yj) + 1)
        area_list.append((area, (i, j)))
# 4776487744

# "Compress" the coordinates by mapping each unique x and y coordinate to an integer index
mapx = {k: v for v, k in enumerate(sorted(set([r[0] for r in data])))}
mapy = {k: v for v, k in enumerate(sorted(set([r[1] for r in data])))}
revertx = {v: k for k, v in mapx.items()}
reverty = {v: k for k, v in mapy.items()}
data2 = [(mapx[x], mapy[y]) for x, y in data]
data = data2
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
area_sorted = sorted(area_list, key=lambda x: x[0], reverse=True)
# area_sorted = sorted(area_list, reverse=True)
max_area2 = 0
for area, (i, j) in area_sorted:
    x1, y1 = data[i]
    x2, y2 = data[j]
    if x1 > x2:
        x1, x2 = x2, x1
    if y1 > y2:
        y1, y2 = y2, y1
    x2 += 1
    y2 += 1
    if (  # Check if the contour of the rectangle is completely inside the shape S
        np.all(minx[y1:y2] <= x1)
        and np.all(maxx[y1:y2] >= x2)
        and np.all(miny[x1:x2] <= y1)
        and np.all(maxx[y1:y2] >= x2)
    ):
        max_area2 = area
        break
area_sorted[0][0], max_area2
# 1560299548
