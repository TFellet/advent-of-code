from bisect import bisect_left

data = open("2025/inputs/day5.txt").read().strip().split("\n\n")
ranges, ids = (d.split("\n") for d in data)
rt = [list(map(int, r.split("-"))) for r in ranges]

# Sort and reduce number of ranges by merging overlapping ones
srt = sorted(rt, key=lambda x: x[0])
new_ranges = [srt[0]]
for r in srt[1:]:
    nr = new_ranges[len(new_ranges)-1]
    # Ranges are sorted, so r[0] is always >= nr[0]
    if r[0] <= nr[1]: # If the left bound is in the current range
        nr[1] = max(nr[1], r[1]) # Extend the current range to the max right bound of both ranges
    else:
        new_ranges.append(r)

# Use binary search to find if a number belongs to a range
def in_range(target, ranges, starts) -> bool:
    i = bisect_left(starts, target) - 1
    return ranges[i][0] <= target <= ranges[i][1]

starts = [x[0] for x in new_ranges]
sum(in_range(int(id), new_ranges, starts) for id in ids) # Part 1
sum(r[1] - r[0] + 1 for r in new_ranges) # Part 2
