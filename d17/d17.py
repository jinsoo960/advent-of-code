from enum import Enum
from math import inf


class Directions(Enum):
    LEFT = (0, -1)
    RIGHT = (0, 1)
    UP = (-1, 0)
    DOWN = (1, 0)

    def __lt__(self, other):
        return False


# def notOpposite(dir1, dir2):
#     return addTuples(dir1.value, dir2.value) != (0, 0)
#
#
# def getNextDirections(dir):
#     return filter(lambda x: notOpposite(x, dir), Directions)

nextDirections = {Directions.LEFT: [Directions.LEFT, Directions.UP, Directions.DOWN],
                  Directions.RIGHT: [Directions.RIGHT, Directions.UP, Directions.DOWN],
                  Directions.UP: [Directions.LEFT, Directions.UP, Directions.RIGHT],
                  Directions.DOWN: [Directions.LEFT, Directions.RIGHT, Directions.DOWN]
                  }


def addTuples(a, b):
    return tuple(map(sum, zip(a, b)))


# dpDict contains the least cost of a path of length pathLen
# starting at yx with direction dir and using strLine straight line
def bfHelper(grid, n, m, yx, dir, pathLen, strLine, dpArray):
    # key = (yx, dir, pathLen, strLine)
    # if key in dpDict:
    #     return dpDict[key]
    key = (dir, pathLen, strLine)
    x = yx[1]
    y = yx[0]

    dpDict = dpArray[y][x]

    if key in dpDict:
        return dpDict[key]

    if pathLen == 0:
        if x == m - 1 and y == n - 1:
            val = 0
        else:
            val = inf
        dpDict[key] = val
        return val

    candidates = []

    for nextDir in nextDirections[dir]:
        if nextDir is dir:
            if strLine == 2:
                continue
            else:
                newStrLine = strLine + 1
        else:
            newStrLine = 0

        nextYx = addTuples(yx, nextDir.value)

        ny = nextYx[0]
        nx = nextYx[1]

        if ny < 0 or ny >= n or nx < 0 or nx >= m:
            continue

        c = bfHelper(grid, n, m, nextYx, nextDir, pathLen - 1, newStrLine, dpArray)

        candidates.append(c + grid[nextYx[0]][nextYx[1]])

    val = min(candidates)

    dpDict[key] = val
    return val


# variation of bellman ford with direction and straight line info
def bf(grid):
    dpDict = {}
    yx = (0, 0)
    strLine = 0
    n = len(grid)
    m = len(grid[0])
    pathLen = n * m
    # pathLen = 100

    dpArray = [[{} for _ in range(m)] for _ in range(n)]

    candidates = []
    for dir in [Directions.RIGHT, Directions.DOWN]:
        for i in range(pathLen):
            c = bfHelper(grid, n, m, yx, dir, i, strLine, dpArray)
            candidates.append(c)

    return min(candidates)


# bellman ford too slow, lift the graph and do dikjstra
def getNeighbors(n, m, yx, dir, strLine):
    neighbors = []
    for nextDir in nextDirections[dir]:
        if nextDir is dir:
            if strLine == 2:
                continue
            else:
                newStrLine = strLine + 1
        else:
            newStrLine = 0

        nextYx = addTuples(yx, nextDir.value)

        ny = nextYx[0]
        nx = nextYx[1]

        if ny < 0 or ny >= n or nx < 0 or nx >= m:
            continue

        neighbors.append((nextYx, nextDir, newStrLine))
    return neighbors


def getNeighborsUltra(n, m, yx, dir, strLine):
    neighbors = []
    for nextDir in nextDirections[dir]:
        if nextDir is dir:
            if strLine == 9:
                continue
            else:
                newStrLine = strLine + 1
        else:
            if strLine < 3:
                continue
            else:
                newStrLine = 0

        nextYx = addTuples(yx, nextDir.value)

        ny = nextYx[0]
        nx = nextYx[1]

        if ny < 0 or ny >= n or nx < 0 or nx >= m:
            continue

        neighbors.append((nextYx, nextDir, newStrLine))
    return neighbors



import heapq as h


def dijkstra(grid):
    n = len(grid)
    m = len(grid[0])

    visited = [[{} for _ in range(m)] for _ in range(n)]

    toVisit = [(0, (0, 0), Directions.RIGHT, 0), (0, (0, 0), Directions.DOWN, 0)]
    visited[0][0][(Directions.RIGHT, 0)] = 0
    visited[0][0][(Directions.DOWN, 0)] = 0

    h.heapify(toVisit)

    while toVisit:
        current = h.heappop(toVisit)
        d = current[0]
        yx = current[1]
        dir = current[2]
        strLine = current[3]

        for neighbor in getNeighborsUltra(n, m, yx, dir, strLine):
            nDir = neighbor[1]
            nStrLine = neighbor[2]

            key = (nDir, nStrLine)
            newY = neighbor[0][0]
            newX = neighbor[0][1]
            if key in visited[newY][newX]:
                continue

            newD = d + grid[newY][newX]
            visited[newY][newX][key] = newD

            val = (newD, neighbor[0], neighbor[1], neighbor[2])
            h.heappush(toVisit, val)

    d = visited[n-1][m-1]

    return min(d[i] for i in d if i[1] >= 3)


def main():
    filename = "input"
    with open(filename) as f:
        grid = [line.strip() for line in f.readlines()]

    grid = [list(map(int, line)) for line in grid]

    output = dijkstra(grid)
    print(output)


if __name__ == "__main__":
    main()
