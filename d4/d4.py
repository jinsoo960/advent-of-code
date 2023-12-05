def part1(lines):
    points = 0
    for line in lines:
        numbers = line[line.index(":") + 1:].split()
        i = numbers.index('|')

        winningNumbers = set(numbers[:i])
        yourNumbers = numbers[i+1:]

        match = sum((n in winningNumbers) for n in yourNumbers)

        if match > 0:
            points += 2 ** (match - 1)

    return points


def part2(lines):
    numCards = [1] * len(lines)
    for j, line in enumerate(lines):
        numbers = line[line.index(":") + 1:].split()
        i = numbers.index('|')

        winningNumbers = set(numbers[:i])
        yourNumbers = numbers[i+1:]

        match = sum((n in winningNumbers) for n in yourNumbers)

        for k in range(j + 1, j + match + 1):
            numCards[k] += numCards[j]

    return sum(numCards)


def main():
    with open("input") as f:
        lines = f.readlines()

    lines = [l.strip() for l in lines]

    output = part2(lines)
    print(output)


if __name__ == "__main__":
    main()
