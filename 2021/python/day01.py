from time import perf_counter_ns

import numpy as np

a2 = np.loadtxt('2021/inputs/day1.txt', dtype=int)
((a2[:-1] < a2[1:]).sum())
((a2[:-3] < a2[3:]).sum())

import polars as pl

a3 = pl.read_csv('2021/inputs/day1.txt', has_header=False, new_columns=["c1"], schema_overrides=[pl.UInt16()], n_threads=1)
a3.select(pl.col("c1").shift(1) < pl.col("c1")).sum()
a3.select(pl.col("c1").shift(3) < pl.col("c1")).sum()

ntot = a3.count().item(0,0)

def pltrans(i):
    ai = pl.read_csv('2021/inputs/day1.txt', has_header=False, new_columns=["c1"], schema_overrides=[pl.UInt16()], n_threads=1, n_rows=i*10)
    ai.select(pl.col("c1").shift(1) < pl.col("c1")).sum()
    ai.select(pl.col("c1").shift(3) < pl.col("c1")).sum()

def nptrans(i):
    ai = np.loadtxt('2021/inputs/day1.txt', dtype=int, max_rows=i*10)
    ((ai[:-1] < ai[1:]).sum())
    ((ai[:-3] < ai[3:]).sum())

times_polars = []
nrows = []
for i in (range(int(ntot/10))):
    t0 = perf_counter_ns()
    for i in range(100):
        pltrans(i)
    t1 = perf_counter_ns()
    times_polars.append((t1-t0)/(100*1000))
    nrows.append(i*10)

times_polars_df = pl.DataFrame({"nrow": [i*10 for i in range(int(ntot/10))], "time": times_polars})
import plotnine as p

(p.ggplot(times_polars_df, p.aes(x="nrow", y="time")) + p.geom_line())

times_np = []
nrows = []
for i in range(int(ntot/10)):
    t0 = perf_counter_ns()
    for i in range(100):
        nptrans(i)
    t1 = perf_counter_ns()
    times_np.append((t1-t0)/(100*1000))
    nrows.append(i*10)

times_np_df = pl.DataFrame({"nrow": [i*10 for i in range(int(ntot/10))], "time": times_np})
(p.ggplot(times_np_df, p.aes(x="nrow", y="time")) + p.geom_line())

