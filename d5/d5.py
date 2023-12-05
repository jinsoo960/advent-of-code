def splitList(l, x):
    start = 0
    splited = []
    for i, y in enumerate(l):
        if y == x:
            if i - start > 0:
                splited.append(l[start:i])
            start = i + 1
    if start < len(l):
        splited.append(l[start:])
    return splited


def constructMap(mapInfo):
    mapName = mapInfo[0].split()[0]
    mapName = mapName.split('-')

    mapSource, mapDest = mapName[0], mapName[2]

    mapList = []

    for l in mapInfo[1:]:
        l = l.split()
        dRangeStart, sRangeStart, rLength = int(l[0]), int(l[1]), int(l[2])
        mapList.append((sRangeStart, rLength, dRangeStart))

    return mapSource, mapDest, sorted(mapList)


def part1(lines):
    l = lines[0]
    seeds = [int(n) for n in l.split()[1:]]

    maps = lines[1:]

    splitted = splitList(maps, '')

    for l in splitted:
        ms, md, ml = constructMap(l)
        for i, seed in enumerate(seeds):
            for sr, rl, dr in ml:
                if seed >= sr and seed < sr + rl:
                    seeds[i] = dr + (seed - sr)
                    break
    return min(seeds)


# interval of form [a, b)
def intersectIntervals(i1, i2):
    i3 = (max(i1[0], i2[0]), min(i1[1], i2[1]))
    if i3[0] >= i3[1]:
        return None
    else:
        return i3


def getIntersection(a, b):
    if a >= b:
        return None
    else:
        return (a, b)


def part2(lines):
    l = lines[0]
    seeds = [int(n) for n in l.split()[1:]]
    seedRanges = []
    for i in range(len(seeds)//2):
        rl = seeds[2*i + 1]
        sr = seeds[2*i]
        seedRanges.append((sr, sr + rl))

    maps = lines[1:]
    splitted = splitList(maps, '')

    for l in splitted:
        ms, md, ml = constructMap(l)

        newRanges = []
        seedRange = None
        for i, seedRange in enumerate(seedRanges):
            for sr, rl, dr in ml:
                sourceRange = (sr, sr + rl)
                intersection = intersectIntervals(seedRange, sourceRange)

                if intersection is None:
                    continue

                newRanges.append((dr + (intersection[0] - sr), dr + (intersection[1] - sr)))

                smallRange = getIntersection(seedRange[0], intersection[0])
                if smallRange is not None:
                    newRanges.append(smallRange)

                seedRange = getIntersection(intersection[1], seedRange[1])

                if seedRange is None:
                    break

            if seedRange is not None:
                newRanges.append(seedRange)

        seedRanges = newRanges

    return min(a for a, _ in seedRanges)



def main():
    with open("input") as f:
        lines = f.readlines()

    lines = [l.strip() for l in lines]

    output = part2(lines)

    print(output)


if __name__ == "__main__":
    main()
