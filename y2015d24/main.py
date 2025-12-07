#!/usr/bin/env python
from collections import defaultdict
import fileinput
import math
from typing import Sequence


def make_table(nums: Sequence[int]):
    # table[n][T] = fewest items needed to sum to T using nums[0..n]
    init = defaultdict[int, int](lambda: math.inf)
    init[nums[0]] = 1
    table = [init]
    for i, num in enumerate(nums):
        if i == 0:
            continue
        # we can always just not use this item
        next = table[-1].copy()
        next[num] = 1
        for s, items in table[-1].items():
            next[s + num] = min(next[s + num], 1 + items)
        table.append(next)
    return table


def min_sum_sets(nums: Sequence[int], big_target: int, slack=5):
    t = make_table(nums)

    def help(i: int, target: int):
        if target == 0:
            return [[]]
        if t[i].get(target) == math.inf:
            return []
        num = nums[i]
        if i == 0:
            if t[i][target] == 1:
                return [[num]]  # exactly one way
            else:
                return []  # impossible

        min_items = t[i][target]
        choices = []
        # comment out this "if" to get _all_ ways to sum to target.
        if t[i - 1][target] == min_items:
            # It's OK to _not_ take this item.
            choices = help(i - 1, target)
        if num == target:
            choices.append([num])
        elif t[i - 1][target - num] == min_items - 1:
            # taking this item is a possibility
            subchoices = help(i - 1, target - num)
            for sc in subchoices:
                choices.append([num, *sc])
        return choices

    return help(len(nums) - 1, big_target)


def is_valid_set(
    all_nums: Sequence[int], santa_nums: Sequence[int], target: int
) -> bool:
    """Can all_nums be partitioned into sums-to-target sets after removing santa_nums?"""
    sns = set(santa_nums)
    nums = [n for n in all_nums if n not in sns]
    t = make_table(nums)
    return t[-1][target] < math.inf


if __name__ == "__main__":
    nums = [int(x) for x in fileinput.input()]
    assert sum(nums) % 3 == 0
    target = sum(nums) // 3
    print(len(nums), target)
    assert nums == [*sorted(nums)]  # list is sorted
    assert len(set(nums)) == len(nums)  # all numbers are unique
    t = make_table(nums)
    # for i, row in enumerate(t):
    #     print(i, sorted([*row.items()]))
    print(t[-1][target])
    santa_sets = min_sum_sets(nums, target)
    print(len(santa_sets))
    valid_santa_sets = [ss for ss in santa_sets if is_valid_set(nums, ss, target)]
    print(len(valid_santa_sets))

    valid_santa_sets.sort(key=math.prod)
    print(math.prod(valid_santa_sets[0]))
