# inGroup = 0 => not in group and seen .
# inGroup = 1 => not in group but did not encounter . yet
# inGroup = 2 => in a group
def dp(field, n, i, c, groups, inGroup, dpDict):
    if (i, c, inGroup, groups) in dpDict:
        return dpDict[(i, c, inGroup, groups)]

    val = 0
    if i == n:
        if not groups:
            val = 1
        else:
            val = 0
        dpDict[(i, c, groups)] = val
        return val

    if field[i] == '?' and c == '?':
        val1 = dp(field, n, i, '#', groups, inGroup, dpDict)
        val2 = dp(field, n, i, '.', groups, inGroup, dpDict)
        dpDict[(i, '#', inGroup, groups)] = val1
        dpDict[(i, '.', inGroup, groups)] = val2
        return val1 + val2

    if c == '.':
        if inGroup == 2:
            val = 0
        else:
            val = dp(field, n, i + 1, field[i+1], groups, 0, dpDict)
    elif c == '#':
        if inGroup == 1:
            val = 0
        else:
            temp = list(groups)
            if not temp:
                val = 0
            else:
                temp[0] -= 1
                if temp[0] == 0:
                    newGroup = tuple(temp[1:])
                    ig = 1
                else:
                    newGroup = tuple(temp)
                    ig = 2
                val = dp(field, n, i + 1, field[i+1], newGroup, ig, dpDict)

    dpDict[(i, c, inGroup, groups)] = val
    return val


def part1(file):
    vals = []
    for line in file:
        line = line.split(' ')
        field = line[0]
        groups = tuple(int(i) for i in line[1].split(','))

        field = [field] * 5
        field = '?'.join(field)
        groups = groups * 5

        a = {}
        extfield = "." + field + "."
        vals.append(dp(extfield, len(field) + 1, 0, '.', groups, 0, a))

    print(vals)
    return sum(vals)


def main():
    filename = "input"
    with open(filename) as f:
        file = [line.strip() for line in f.readlines()]

    output = part1(file)
    print(output)


if __name__ == "__main__":
    main()
