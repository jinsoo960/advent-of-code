import numpy as np

charToDir = {
        'R': np.array([0, 1]),
        'L': np.array([0, -1]),
        'U': np.array([-1, 0]),
        'D': np.array([1, 0])
        }


intToDir = ['R', 'D', 'L', 'U']


def parseLine(line):
    contents = line.split()

    color = contents[2][1:-1]

    c = contents[0]
    return c, int(contents[1]), color


def parseLine2(line):
    _, _, inst = parseLine(line)

    dir = intToDir[int(inst[-1])]
    n = int(inst[1:-1], 16)
    return dir, n, None


# class for a line
# vertical lines do not possess its end points
# so that each point has unique line containing it
class Line:
    def __init__(self, startPoint, n, dirIn, dirPrev, dirNext):
        endPoint = startPoint + charToDir[dirIn] * n
        self.x1 = min(startPoint[1], endPoint[1])
        self.x2 = max(startPoint[1], endPoint[1])

        self.y1 = min(startPoint[0], endPoint[0])
        self.y2 = max(startPoint[0], endPoint[0])

        self.isHorizontal = dirIn == 'L' or dirIn == 'R'
        self.isTurn = dirPrev != dirNext

    def getX1(self):
        return self.x1

    def getX2(self):
        return self.x2

    # vertical line does not contain its end points
    # does not matter if we get y1 > y2 as the result
    def getY1(self):
        if not self.isHorizontal:
            return self.y1 + 1
        else:
            return self.y1

    def getY2(self):
        if not self.isHorizontal:
            return self.y2 - 1
        else:
            return self.y2


def makeLines(digPlan):
    origin = np.array([0, 0])
    currentYx = origin

    k = len(digPlan)

    lines = []

    for i, dp in enumerate(digPlan):
        c = dp[0]
        n = dp[1]

        dir = charToDir[c]

        lines.append(Line(currentYx, n, c, digPlan[(i - 1) % k][0], digPlan[(i + 1) % k][0]))

        currentYx += dir * n

    return lines


# count interior point of a row
def countIntRow(lines, row):
    # filter out only the lines that intersect the row
    intersectingLines = filter(lambda line: line.getY1() <= row and line.getY2() >= row, lines)

    inside = False
    count = 0
    lastX = 0
    for line in intersectingLines:
        # if inside the interior add the number of points between two lines
        if inside:
            count += line.getX1() - lastX

        # add the number of points consisting a line
        count += line.getX2() - line.getX1() + 1
        lastX = line.getX2() + 1

        # vertical line is always a boundary
        if not line.isHorizontal:
            inside = not inside
        else:
            # horizontal lines are a boundary only if they are not \cup or \cap shape
            if not line.isTurn:
                inside = not inside
    return count


def countInt(lines):
    # find all places new horizontal lines appear
    ys = []
    for line in lines:
        if line.isHorizontal:
            ys.append(line.getY1())
    ys = sorted(set(ys))

    count = 0
    for i in range(len(ys) - 1):
        # count interior for the line with a horizontal line
        c = countIntRow(lines, ys[i])
        count += c

        # count interior for the next line
        # note that the number of interior points stays constant
        # until the next horizontal line occurs
        cc = countIntRow(lines, ys[i] + 1)
        count += cc * (ys[i + 1] - (ys[i] + 1))

    # for the last horizontal line
    c = countIntRow(lines, ys[-1])
    count += c
    return count


def countIntNaive(lines):
    minY = min(line.getY1() for line in lines)
    maxY = max(line.getY2() for line in lines)

    count = sum(countIntRow(lines, i) for i in range(minY, maxY + 1))
    return count


def main():
    filename = "input"
    with open(filename) as f:
        file = [line.strip() for line in f.readlines()]

    digPlan = [parseLine2(l) for l in file]

    lines = makeLines(digPlan)

    lines = sorted(lines, key=lambda line: line.getX1())

    count = countInt(lines)
    print(count)


if __name__ == "__main__":
    main()
