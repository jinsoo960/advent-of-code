import numpy as np
from itertools import combinations


def emptyRowsCols(galaxy):
    emptyRows = [i for i in range(galaxy.shape[0]) if not np.isin('#', galaxy[i, :]).all()]
    emptyCols = [i for i in range(galaxy.shape[1]) if not np.isin('#', galaxy[:, i]).all()]
    return emptyRows, emptyCols


def expandGalaxy(galaxy):
    emptyRows, emptyCols = emptyRowsCols(galaxy)
    galaxy = np.insert(galaxy, emptyRows, '.', axis=0)
    galaxy = np.insert(galaxy, emptyCols, '.', axis=1)

    return galaxy


def setGalaxyNumbers(galaxy):
    return np.argwhere(galaxy == '#')


def distance(loc1, loc2):
    (x1, y1) = loc1
    (x2, y2) = loc2
    return np.abs(x1 - x2) + np.abs(y1 - y2)


def part1(file):
    galaxy = np.array(file)

    expanded = expandGalaxy(galaxy)

    locations = setGalaxyNumbers(expanded)

    return sum(distance(l1, l2) for l1, l2 in combinations(locations, 2))


def distanceEmpty(loc1, loc2, emptyRows, emptyCols, age):
    def helper(a, b, arr):
        return np.sum((arr > a) & (arr < b))

    (x1, y1) = loc1
    (x2, y2) = loc2
    nxEmpty = helper(min(x1, x2), max(x1, x2), emptyRows)
    nyEmpty = helper(min(y1, y2), max(y1, y2), emptyCols)

    return np.abs(x1 - x2) - nxEmpty + np.abs(y1 - y2) - nyEmpty + age * (nxEmpty + nyEmpty)


def part2(file, age):
    galaxy = np.array(file)
    locations = setGalaxyNumbers(galaxy)
    emptyRows, emptyCols = emptyRowsCols(galaxy)

    return sum(distanceEmpty(l1, l2, emptyRows, emptyCols, age) for l1, l2 in combinations(locations, 2))


def main():
    filename = "input"
    with open(filename) as f:
        file = f.readlines()
        file = [list(line.strip()) for line in file]
    age = 1000000

    # output = part1(file)
    output = part2(file, age)
    print(output)


if __name__ == "__main__":
    main()
