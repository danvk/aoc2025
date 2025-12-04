#!/usr/bin/env python
lines = [x.strip() for x in open('input/2015/day19/input.txt').readlines()]
molecule = lines[-1]
productions = [tuple(line.split(' => ')) for line in lines[:-2]]

print(molecule)
print(productions)

assert len({rhs for _, rhs in productions}) == len(productions)
rhs_to_prev = {rhs: lhs for lhs, rhs in productions}
assert len(rhs_to_prev) == len(productions)

easyforces = [(lhs, rhs) for lhs, rhs in productions if 'Rn' in rhs and 'Ar' in rhs]
normals = [(lhs, rhs) for lhs, rhs in productions if 'Rn' not in rhs and lhs != 'e']
atoms = set(lhs for lhs, _ in productions)
print(f'{len(atoms)=}', atoms)
print(f'{len(normals)=}', normals)
# print(len(easyforces))
# print(len(productions))

def parse_molecule(molecule: str):
    if not molecule:
        return []
    if molecule[0].isupper() and len(molecule) > 1 and molecule[1].islower():
        return [molecule[:2]] + parse_molecule(molecule[2:])
    return [molecule[0]] + parse_molecule(molecule[1:])

repMap = dict[str, list[list[str]]]()
for lhs, rhs in normals:
    repMap.setdefault(lhs, [])
    repMap[lhs].append(parse_molecule(rhs))


targets = {''.join(parse_molecule(rhs)[2:-1]) for _, rhs in easyforces}
print(f'{targets=}')

def apply_replacement(molecule: list[str]):
    if not molecule:
        return []
    x = molecule[0]
    out = [[x] + rest for rest in apply_replacement(molecule[1:])]
    replaceIt = []
    if x in repMap:
        replaceIt = [rep + molecule[1:] for rep in repMap[x]]
    return out + replaceIt


def step(reps: list[str, str]) -> list[str, str]:
    out = list[str, str]()
    for lhs, rhs in reps:
        mol = parse_molecule(rhs)
        for next in apply_replacement(mol):
            out.append((lhs, ''.join(next)))
    return out


def find_leaf(molecule: str):
    """Find the shortest 'leaf' Rn..Ar span."""
    spans = []
    last_rn = -1
    for i in range(len(molecule)):
        if molecule[i:i+2] == 'Rn':
            last_rn = i
        elif molecule[i:i+2] == 'Ar':
            if last_rn != -1:
                spans.append((last_rn, i + 2))
                last_rn = -1
    return min(spans, key=lambda x: x[1] - x[0])


def all_matches(haystack: str, needle: str):
    indices = []
    start = 0
    while True:
        index = haystack.find(needle, start)
        if index == -1:
            break
        indices.append(index)
        start = index + 1  # Start searching from the character after the found substring
    return indices


def steps_to_target(molecules: set[str], targets: set[str]):
    assert molecules
    print(len(molecules))
    print(min(len(m) for m in molecules))
    if not molecules.isdisjoint(targets):
        match = [*(molecules & targets)][0]
        return [match]
    prevs = {}
    for molecule in molecules:
        for rhs, lhs in rhs_to_prev.items():
            for idx in all_matches(molecule, rhs):
                prev = molecule[:idx] + lhs + molecule[idx+len(rhs):]
                prevs[prev] = molecule
    all_prevs = set(prevs.keys())
    seq = steps_to_target(all_prevs, targets)
    return [prevs[seq[0]]] + seq

print(targets)
print(steps_to_target({'CaCaSi'}, {'Si'}))
print(steps_to_target({'SiAlYSiAl'}, targets))
print(steps_to_target({'FYCaPTiBCaSiThCaSiThPMg'}, targets))
print(steps_to_target({'TiBSiThCaCaSiThCaF'}, targets))

easyforces.append(('CaCa', 'CaCaCa'))
easyforces.append(('TiTi', 'TiTiTi'))

num = 0
while True:
    print(len(molecule), molecule)
    is_match = False
    for lhs, rhs in easyforces:
        if rhs in molecule:
            print('  ', rhs, ' -> ', lhs)
            is_match = True
            num += 1
            molecule = molecule.replace(rhs, lhs, 1)
    # start = molecule
    # molecule = re.sub(r'Ti(Ti)+Ti', 'TiTi', molecule)
    # molecule = re.sub(r'Ca(Ca)+Ca', 'CaCa', molecule)
    # if molecule != start:
    #     is_match = True
    #     num += 1
    #     print('  (regex)')
    if not is_match and 'Rn' in molecule:
        start, stop = find_leaf(molecule)
        # molecule[start:stop] = Rn..Ar
        inner = molecule[start+2:stop-2]
        did_the_y = False
        if 'Y' in inner and len(inner) > 10:
            y = inner.index('Y')
            stop = start+2+y
            print('   ', inner)
            inner = molecule[start+2:stop]
            if len(inner) > 10:
                did_the_y = True
                print(' ->', inner)
                steps = steps_to_target({inner}, {'F', 'Mg'})
                num += (len(steps) - 1)
                print('  inner steps', len(steps) - 1, steps)
                # print('    ', molecule)
                molecule = molecule[:start+2] + steps[-1] + molecule[stop:]
                # print('  ->', molecule)
        if not did_the_y:
            start, stop = find_leaf(molecule)
            # molecule[start:stop] = Rn..Ar
            inner = molecule[start+2:stop-2]
            steps = steps_to_target({inner}, targets)
            num += (len(steps) - 1)
            print('  inner steps', len(steps) - 1, steps)
            # print('    ', molecule)
            molecule = molecule[:start+2] + steps[-1] + molecule[stop-2:]
            # print('  ->', molecule)
        is_match = True

    if not is_match:
        break

print('Backed out to', molecule)
final_steps = steps_to_target({molecule}, {'e'})
print(final_steps)
num += len(final_steps) - 1
print(f'{num=}')

# print(molecule.replace('Rn', '<p>').replace('Ar', '</p>'))
# print(apply_replacement(parse_molecule('HF')))
