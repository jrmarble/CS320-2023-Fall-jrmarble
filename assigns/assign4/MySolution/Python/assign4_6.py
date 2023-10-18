import sys
sys.path.append("./../../../../classlib/Python")
from MyPython import *

def theNatPairs_cubesum():
    pairs = [(0**3 + 0**3, (0, 0))]
    while pairs:
        pairs.sort()
        cube_sum, (i, j) = pairs.pop(0)
        yield (i, j)
        pairs.append((i**3 + (j + 1)**3, (i, j + 1)))
        if i == j:
            pairs.append((((i+1)**3)*2, (i + 1, i + 1)))
