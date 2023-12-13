import numpy as np


def findReflection(x):
    n, m = x.shape

    for i in range(1, m):
        mismatch = False
        for j in range(min(i, m - i)):
            if (x[:, i - j - 1] != x[:, i + j]).any():
                mismatch = True
                break
        if not mismatch:
            return i

    return 0


def findSmudge(x):
    n, m = x.shape

    for i in range(1, m):
        numWrong = 0
        for j in range(min(i, m - i)):
            numWrong += np.sum(x[:, i - j - 1] != x[:, i + j])
            if numWrong > 1:
                break
        if numWrong == 1:
            return i

    return 0


def part1(file):
    output = 0
    for x in file:
        i = findReflection(x)
        output += i
        i = findReflection(x.T)
        output += i * 100
    return output


def part2(file):
    output = 0
    for x in file:
        i = findSmudge(x)
        if i == 0:
            i = findSmudge(x.T) * 100
        output += i
    return output


def main():
    filename = "input"
    with open(filename) as f:
        # file = [line.strip() for line in f.readlines()]
        file = f.read()
        file = file.split("\n\n")
        file = [line.split('\n') for line in file]

    notes = []
    for note in file:
        note = [list(row) for row in note if row != ""]
        notes.append(np.array(note))

    output = part2(notes)
    print(output)


if __name__ == "__main__":
    main()
