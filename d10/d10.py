from itertools import product


def getNextIndex(lines, x, y, offsetX, offsetY):
    c = lines[x][y]
    if c == "|":
        newX = x + offsetX
        newY = y + offsetY
    elif c == "-":
        newX = x + offsetX
        newY = y + offsetY
    elif c == "L":
        newX = x + offsetY
        newY = y + offsetX
    elif c == "7":
        newX = x + offsetY
        newY = y + offsetX
    elif c == "J":
        newX = x + -1 * offsetY
        newY = y + -1 * offsetX
    elif c == "F":
        newX = x + -1 * offsetY
        newY = y + -1 * offsetX
    else:
        raise

    return newX, newY


def findS(lines):
    startPosition = (0, 0)
    for i, l in enumerate(lines):
        try:
            j = l.index("S")
            startPosition = (i, j)
            break
        except ValueError:
            continue
    return startPosition


def validSecond(c, offsetX, offsetY):
    if c == "-":
        return offsetY != 0
    if c == "|":
        return offsetX != 0
    if c == "F":
        return offsetX == -1 or offsetY == -1
    if c == "J":
        return offsetX == 1 or offsetY == 1
    if c == "7":
        return offsetX == -1 or offsetY == 1
    if c == "L":
        return offsetX == 1 or offsetY == -1
    raise


def findSecond(lines, startPosition):
    offsets = [(-1, 0), (1, 0), (0, -1), (0, 1)]
    for x, y in offsets:
        try:
            xx = startPosition[0] + x
            yy = startPosition[1] + y
            c = lines[xx][yy]
            if c != ".":
                if validSecond(c, x, y): 
                    return (xx, yy, x, y)
        except IndexError:
            continue

    return (0, 0, 0, 0)


def findLoop(lines):
    startPosition = findS(lines)
    loop = [findSecond(lines, startPosition)]

    current = lines[loop[0][0]][loop[0][1]]
    while current != "S":
        last = loop[-1]
        x = last[0]
        y = last[1]
        offsetX = last[2]
        offsetY = last[3]

        newX, newY = getNextIndex(lines, x, y, offsetX, offsetY)

        current = lines[newX][newY]

        loop.append((newX, newY, newX - x, newY - y))

    return loop


def part1(lines):
    return len(findLoop(lines)) // 2


def printGrid(grid):
    for l in grid:
        print(''.join(l))


# embed the loop into a grid of twice the size, so the interior is connected
# note that only the even indices exist in the original grid
def embedLoop(lines, loop):
    n = len(lines)
    m = len(lines[0])
    grid = [["."] * (2 * m) for _ in range(2 * n)]

    for x, y, offsetX, offsetY in loop:
        grid[2*x][2*y] = lines[x][y]
        if offsetX != 0:
            grid[2*x - offsetX][2*y] = "|"
        if offsetY != 0:
            grid[2*x][2*y - offsetY] = "-"

    return grid


# find the starting points of interior and exterior of the loop
def findInitials(lines, loop):
    for x, y, _, _ in loop:
        c = lines[x][y]
        if c == "-":
            return (2*x - 1, 2*y), (2*x + 1, 2*y)
        elif c == "|":
            return (2*x, 2*y - 1), (2*x, 2*y + 1)
    return (0, 0), (0, 0)


# if not interior of the loop, return -1
def bfsGrid(grid, start):
    toVisit = [start]
    grid[start[0]][start[1]] = "V"
    count = 0
    while toVisit:
        current = toVisit.pop()
        x, y = current[0], current[1]
        # if the indices are even, there is a corresponding entry in the original grid, so count
        if x % 2 == 0 and y % 2 == 0:
            count += 1

        offsets = [(-1, 0), (1, 0), (0, -1), (0, 1)]

        for offsetX, offsetY in offsets:
            newX = x + offsetX
            newY = y + offsetY
            try:
                if grid[newX][newY] == ".":
                    grid[newX][newY] = "V"
                    toVisit.append((newX, newY))
            except IndexError:
                return -1
    return count


def part2(lines):
    loop = findLoop(lines)

    grid = embedLoop(lines, loop)

    start1, start2 = findInitials(lines, loop)

    # try again if did not start in the interior
    count = bfsGrid(grid, start1)
    if count == -1:
        count = bfsGrid(grid, start2)

    return count


def main():
    filename = "input"
    with open(filename) as f:
        file = f.readlines()
    lines = [line.strip() for line in file]
    output = part2(lines)
    print(output)


if __name__ == "__main__":
    main()
