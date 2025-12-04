import polars as pl
from polars import col as c

# Approach 1: Change the L moves to R moves
df = (
    pl.DataFrame({"c1": "R50"}).vstack(pl.read_csv("2025/inputs/day1.txt", has_header=False, new_columns=["c1"]))
    # First character is L or R, rest is number. Multiply by 1 if R, -1 if L
    .with_columns(num=c.c1.str.slice(1).cast(pl.Int32), sign=pl.when(c.c1.str.starts_with("R")).then(1).otherwise(-1))
    .with_columns(pos=c.num.mul(c.sign).cum_sum() % 100)
    # When moving left, reflect the starting position and move right instead
    .with_columns(pass0=((c.sign*c.pos.shift(1))%100 + c.num)//100)
)
(df["pos"] % 100 == 0).sum()
df["pass0"].sum()

# Approach 2: Deal with overcounted and undercounted values
df = (
    pl.read_csv("2025/inputs/day1.txt", has_header=False, new_columns=["c1"])
    .with_columns(
        # First character is L or R, rest is number. Multiply by 1 if R, -1 if L
        num=c.c1.str.slice(1).cast(pl.Int32) * pl.when(c.c1.str.starts_with("R")).then(1).otherwise(-1)
    )
    .with_columns(pos=c.num.cum_sum().add(50)) # Start position is 50, then add each move
    .with_columns(prev=c.pos.shift(1, fill_value=50)) # Previous position
    .with_columns(diff_mod=(c.prev//100 - c.pos//100).abs(), # Number of times 0 is crossed
                  # When the dial goes to 0 from the right, the last pass over 0 wasn't counted
                  # ex: Going from 50 to 0 should be 1 crossing, but the diff_mod is 0
                  undercounted=(c.pos % 100 == 0) & (c.prev > c.pos),
                  # When the dial was at 0 and it goes left, it overcounts 1
                  # ex: going from 0 to -50 should be 0 crossing, but the diff_mod is 1
                  overcounted=(c.prev % 100 == 0) & (c.pos < c.prev))
)
# Number of times the dial stops at 0
(df["pos"] % 100 == 0).sum()
# Number of times the dial goes through 0
df["diff_mod"].sum() + df["undercounted"].sum() - df["overcounted"].sum() # type: ignore

# Pure Python with reflection of left moves
data = open("2025/inputs/day1.txt").readlines()
p1 = p2 = 0
pos = 50
for inst in data:
    sign = 1 if inst[0] == "R" else -1
    n = int(inst[1:])
    p2 += (((pos*sign)%100)+n)//100
    pos = (pos+n*sign)%100
    p1 += pos==0
(p1, p2)

