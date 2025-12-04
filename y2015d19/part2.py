#!/usr/bin/env python
import re

lines = [x.strip() for x in open('input/2015/day19/input.txt').readlines()]
molecule = lines[-1]
productions = [tuple(line.split(' => ')) for line in lines[:-2]]

print(molecule)
print(productions)

assert len({rhs for _, rhs in productions}) == len(productions)

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


# TODO: need to unique these by shortest path
shorts = step(normals)
shorts2 = step(shorts)
shorts3 = step(shorts2)
shorts4 = step(shorts3)
shorts5 = step(shorts4)


for lhs, rhs in normals + shorts + shorts2 + shorts3 + shorts4 + shorts5:
    easyforces.append((f'Rn{lhs}Ar', f'Rn{rhs}Ar'))
    easyforces.append((f'Y{lhs}Ar', f'Y{rhs}Ar'))
    easyforces.append((f'Rn{lhs}Y', f'Rn{rhs}Y'))
    easyforces.append((f'Y{lhs}Y', f'Y{rhs}Y'))

assert ('RnFAr', 'RnPMgAr') in easyforces


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
    start = molecule
    molecule = re.sub(r'Ti(Ti)+Ti', 'TiTi', molecule)
    molecule = re.sub(r'Ca(Ca)+Ca', 'CaCa', molecule)
    if molecule != start:
        is_match = True
        num += 1
        print('  (regex)')
    if not is_match:
        break

print(molecule)
print(molecule.replace('Rn', '<p>').replace('Ar', '</p>'))


print(apply_replacement(parse_molecule('HF')))
