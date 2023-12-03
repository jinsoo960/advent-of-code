from collections import Counter

def stripSplit(s, delim):
    return [x.strip() for x in s.split(delim)]

maxCount = Counter(blue = 14, red = 12, green = 13)

def countSet(s):
    cubes = stripSplit(s, ',')
    counter = Counter()
    for c in cubes:
        numColor = stripSplit(c, ' ')
        n = int(numColor[0])
        color = numColor[1]

        counter[color] += n

    return counter <= maxCount


def countGame(s):
    i = s.index(':')
    sets = stripSplit(s[i + 1:], ';')
    return all(map(countSet, sets))

def part1(lines):
    n = 0
    for i, l in enumerate(lines):
        if countGame(l):
            n += i + 1
    return n

def fitSet(s):
    cubes = stripSplit(s, ',')
    counter = Counter()
    for c in cubes:
        numColor = stripSplit(c, ' ')
        n = int(numColor[0])
        color = numColor[1]

        counter[color] += n

    return counter

def fitGame(s):
    i = s.index(':')
    sets = stripSplit(s[i + 1:], ';')

    counter = Counter()
    for set_ in sets:
        c = fitSet(set_)
        counter |= c

    return counter["blue"] * counter["red"] * counter["green"]

def part2(lines):
    return sum(map(fitGame, lines))

if __name__ == "__main__":
    with open("input") as f:
        lines = f.readlines()

    print(part2(lines))
