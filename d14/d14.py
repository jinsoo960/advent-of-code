import numpy as np


def getSplitIndex(arr, delim):
    indices = []
    start = 0
    for i, c in enumerate(arr):
        if c == delim:
            indices.append((start, i))
            start = i + 1
    indices.append((start, len(arr)))
    indices = list(filter(lambda x: x[0] != x[1], indices))
    return indices


def calculateLoad(arr, start, end):
    count = np.sum(arr[start:end] == 'O')
    n = len(arr)
    load = 0
    for i in range(count):
        load += n - i - start
    return load


def rollPlatform(platform, indices):
    n, m = platform.shape
    for i in range(m):
        for start, end in indices[i]:
            count = 0
            for k in range(start, end):
                if platform[k, i] == 'O':
                    count += 1
                    platform[k, i] = '.'
            # arr = platform[:, i]
            # count = np.sum(arr[start:end] == 'O')

            # platform[start:end, i] = '.'
            platform[start:start + count, i] = 'O'
    return platform


def part1(platform):
    n, m = platform.shape
    load = 0

    for i in range(m):
        arr = platform[:, i]
        indices = getSplitIndex(arr, '#')
        l = sum(calculateLoad(arr, index[0], index[1]) for index in indices)
        load += l

    return load


def calculateLoadNoTilt(platform):
    n, m = platform.shape
    load = 0
    for i in range(n):
        count = np.sum(platform[i, :] == 'O')
        load += count * (n - i)
    return load


def arrayToString(arr):
    return ''.join(''.join(c for c in line) for line in arr)


# find out the period of the cycles to avoid computing the whole thing
def part2(platform):
    n = 1000
    _, m = platform.shape
    indicess = []
    for i in range(4):
        p = np.rot90(platform, -i)
        indicess.append(list(getSplitIndex(p[:, j], '#') for j in range(m)))

    seen = {}
    period = 0

    for i in range(n):
        for indices in indicess:
            platform = rollPlatform(platform, indices)
            platform = np.rot90(platform, -1)
        a = arrayToString(platform)

        if a in seen:
            period = i - seen[a][0]
            break
        else:
            seen[a] = (i, calculateLoadNoTilt(platform))

    initial = seen[a][0]

    n = 1000000000 - 1
    k = (n - initial) % period
    for _, value in seen.items():
        if value[0] - initial == k:
            return value[1]

    return calculateLoadNoTilt(platform)


def main():
    filename = "input"
    with open(filename) as f:
        file = [list(line.strip()) for line in f.readlines()]

    platform = np.array(file)

    output = part2(platform)
    print(output)


if __name__ == "__main__":
    main()
