#!/usr/bin/env python

from collections import defaultdict
import fileinput

edges = {}
nodes = set()
inverted = defaultdict[str, list](list)
for line in fileinput.input():
    n, outs = line.strip().split(": ")
    edges[n] = outs.split(" ")
    nodes.add(n)
    nodes.update(edges[n])
    for node in edges[n]:
        inverted[node].append(n)


def topo_dfs(
    node: str, edges: dict[str, list[str]], visited: dict[str, int], stack: list[str]
):
    mark = visited.get(node, 0)
    if mark == 1:
        return  # permanent mark
    elif mark == 2:
        raise ValueError("graph has a cycle!")
    visited[node] = 2
    for neighbor in edges.get(node, []):
        topo_dfs(neighbor, edges, visited, stack)
    visited[node] = 1
    stack.append(node)


visited = {}
stack = []
topo_dfs("svr", edges, visited, stack)
assert len(stack) == len(nodes)

stack.reverse()
print("\n".join(stack))
