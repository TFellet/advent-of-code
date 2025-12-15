import numpy as np

data = open("2025/inputs/day12.txt").read().split("\n\n")

# All possible rotations of all shapes pre-computed
shapes = [np.array([s == "#" for s in d[3:].replace("\n", "")]).reshape(3, 3) for d in data[0:6]]
shapes_d = {}
for i in range(len(shapes)):
    shape = shapes[i]
    shape_rots = []
    for k in range(4):
        s = np.rot90(shape, k)
        present = any([True for s1 in shape_rots if np.all(s1 == s)])
        if not present:
            shape_rots.append(s)
        sf = np.fliplr(s)
        present = any([True for s1 in shape_rots if np.all(s1 == sf)])
        if not present:
            shape_rots.append(sf)
    shapes_d[i] = shape_rots
shapes_d
sizes = [np.sum(s) for s in shapes]

# Parse all grids
grids = [
    (tuple(map(int, (gr.split(":")[0].split("x")))), tuple(map(int, gr.split(":")[1].split(" ")[1:])))
    for gr in data[6].strip().split("\n")
]


# Try to insert a specific shape into a canvas
def insert(canvas, shape):
    idx = np.nonzero(canvas)
    maxi = min(max(idx[0], default=0) + 1, canvas.shape[0] - 2)
    maxj = min(max(idx[1], default=0) + 1, canvas.shape[1] - 2)

    idx0 = [(i, j) for i, j in np.argwhere(canvas == 0) if (i < maxi) and (j < maxj)]
    for i, j in idx0:
        if not np.any(np.logical_and(canvas[i : (i + 3), j : (j + 3)], shape)):
            new_canvas = np.copy(canvas)
            new_canvas[i : (i + 3), j : (j + 3)] = shape
            yield new_canvas


def step(canvas, shapes_ids):
    if sum(shapes_ids) == 0:
        return True
    for id, n in enumerate(shapes_ids):
        if n == 0:
            continue
        new_shape_ids = list(shapes_ids)
        new_shape_ids[id] -= 1
        new_shape_ids = tuple(new_shape_ids)
        # Loop on rotations and twists of the shape
        for shape in shapes_d[id]:
            # Loop on all possible placements of the shape
            for new_canvas in insert(canvas, shape):
                if step(new_canvas, new_shape_ids):
                    return True
    return False


p1 = 0
for gr in grids:
    canvas = np.zeros(gr[0])
    shapes_ids = gr[1]
    total_area = gr[0][0] * gr[0][1]
    shape_area = sum([size * n for size, n in zip(sizes, shapes_ids)])
    maximum_shape_area = sum([9 * n for n in shapes_ids])
    p1 += (shape_area <= total_area) and ((maximum_shape_area <= total_area) or step(canvas, shapes_ids))
p1
