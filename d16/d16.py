from enum import Enum


class Directions(Enum):
    LEFT = (0, -1)
    RIGHT = (0, 1)
    UP = (-1, 0)
    DOWN = (1, 0)


def addTuples(a, b):
    return tuple(map(sum, zip(a, b)))


def getNextMove(layout, n, m, move):
    y = move[0][0]
    x = move[0][1]
    dir = move[1]
    dv = dir.value

    nexts = []

    if layout[y][x] == '.':
        nexts.append((addTuples(move[0], dv), dir))

    elif layout[y][x] == '|':
        if dir is Directions.UP or dir is Directions.DOWN:
            nexts.append((addTuples(move[0], dv), dir))
        else:
            for d in [Directions.UP, Directions.DOWN]:
                nexts.append((addTuples(move[0], d.value), d))

    elif layout[y][x] == '-':
        if dir is Directions.LEFT or dir is Directions.RIGHT:
            nexts.append((addTuples(move[0], dv), dir))
        else:
            for d in [Directions.LEFT, Directions.RIGHT]:
                nexts.append((addTuples(move[0], d.value), d))

    elif layout[y][x] == '\\':
        nextDir = (dv[1], dv[0])
        nexts.append((addTuples(move[0], nextDir), Directions(nextDir)))

    elif layout[y][x] == '/':
        nextDir = (-dv[1], -dv[0])
        nexts.append((addTuples(move[0], nextDir), Directions(nextDir)))

    else:
        raise

    return filter(lambda x: x[0][0] >= 0 and x[0][0] < n and x[0][1] >= 0 and x[0][1] < m, nexts)


def part1(file, initial):
    n = len(file)
    m = len(file[0])

    visited = [[set() for _ in range(m)] for _ in range(n)]

    toVisit = [initial]
    visited[initial[0][0]][initial[0][1]].add(initial[1])

    while toVisit:
        current = toVisit.pop()

        nexts = getNextMove(file, n, m, current)

        for yx, d in nexts:
            if d not in visited[yx[0]][yx[1]]:
                visited[yx[0]][yx[1]].add(d)
                toVisit.append((yx, d))

    s = 0
    for i in range(n):
        for j in range(m):
            if visited[i][j]:
                s += 1

    return s


def part2(file):
    n = len(file)
    m = len(file[0])
    es = []
    for i in range(n):
        initial = ((0, i), Directions.DOWN)
        e = part1(file, initial)
        es.append(e)

        initial = ((m - 1, i), Directions.UP)
        e = part1(file, initial)
        es.append(e)

    for i in range(m):
        initial = ((i, 0), Directions.RIGHT)
        e = part1(file, initial)
        es.append(e)

        initial = ((i, n - 1), Directions.LEFT)
        e = part1(file, initial)
        es.append(e)

    return max(es)


def main():
    filename = "input"
    with open(filename) as f:
        file = [line.strip() for line in f.readlines()]

    output = part2(file)
    print(output)


if __name__ == "__main__":
    main()
