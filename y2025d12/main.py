#!/usr/bin/env python

import fileinput

pip_counts = [7, 6, 7, 5, 7, 7]

n_feasible = 0
n_infeasible = 0
for line in fileinput.input():
    if "x" not in line:
        continue
    line = line.strip()
    # 40x48: 34 38 38 31 28 38
    dims, nums = line.split(": ")
    w, h = map(int, dims.split("x"))
    nums = [*map(int, nums.split(" "))]
    blocks = sum(c * n for (c, n) in zip(pip_counts, nums))
    if blocks > w * h:
        print(f"{line} is infeasible")
        n_infeasible += 1
    else:
        n_feasible += 1

print(f"{n_feasible=} {n_infeasible=}")
