#!/usr/bin/env python


from collections import defaultdict
import sys


target = 33100000
n = 1
maxval = 0
terms = defaultdict(int)
while True:
    for i in range(1, 51):
        terms[n*i] += 11 * i
        if terms[n*i] >= target:
            print(n*i)
            sys.exit(0)
    n += 1
    if n % 10000 == 0:
        print(n, len(terms), max(terms.values()))

