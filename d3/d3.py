


# inclusive start end
def findNumbers(line):
    indices = []
    startIndex = -1
    for i in range(len(line)):
        if line[i].isdigit() and startIndex == -1:
            startIndex = i
            endIndex = i
        elif line[i].isdigit():
            endIndex = i
        elif not line[i].isdigit() and startIndex != -1:
            indices.append((startIndex, endIndex))
            startIndex = -1

    return indices

#inclusive start end
def getAdjacentIndices(rowN, colStart, colEnd):
    indices = []
    for i in range(colStart - 1, colEnd + 2):
        indices.append((rowN - 1, i))
        indices.append((rowN + 1, i))
    indices.append((rowN, colStart - 1))
    indices.append((rowN, colEnd + 1))

    return indices

def part1(lines):
    height = len(lines)
    width = len(lines[0])
    sumNumbers = 0
    for i, l in enumerate(lines):
        numbers = findNumbers(l)

        for indices in numbers:
            s = l[indices[0]:indices[1]+1]
            adjIndices = getAdjacentIndices(i, indices[0], indices[1])
            adjIndices = filter(lambda x: x[0] >= 0 and x[0] < height and x[1] >= 0 and x[1] < width, adjIndices)
            for idx in adjIndices:
                c = lines[idx[0]][idx[1]]
                if not c.isdigit() and c != '.':
                    s = l[indices[0]:indices[1]+1]
                    sumNumbers += int(s)
                    break
    return sumNumbers

def part2(lines):
    height = len(lines)
    width = len(lines[0])
    sumNumbers = 0
    gearsMap = {}
    for i, l in enumerate(lines):
        numbers = findNumbers(l)

        for indices in numbers:
            s = l[indices[0]:indices[1]+1]
            adjIndices = getAdjacentIndices(i, indices[0], indices[1])
            adjIndices = filter(lambda x: x[0] >= 0 and x[0] < height and x[1] >= 0 and x[1] < width, adjIndices)
            for idx in adjIndices:
                c = lines[idx[0]][idx[1]]
                if c == '*': 
                    s = int(l[indices[0]:indices[1]+1])
                    if (idx[0], idx[1]) in gearsMap:
                        gearsMap[(idx[0], idx[1])].append(s)
                    else:
                        gearsMap[(idx[0], idx[1])] = [s]
    for _, ns in gearsMap.items():
        if len(ns) == 2:
            sumNumbers += ns[0] * ns[1]

    return sumNumbers



def main():
    with open("input") as f:
        lines = f.readlines()
    lines = [l.strip() + '.' for l in lines]
    print(part2(lines))

if __name__ == "__main__":
    main()
