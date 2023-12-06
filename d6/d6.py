import math


def main():
    with open("input") as f:
        lines = f.readlines()

    lines = [l.strip() for l in lines]

    # times = [int(x) for x in lines[0].split()[1:]]
    # distances = [int(x) for x in lines[1].split()[1:]]

    times = [int(''.join(lines[0].split()[1:]))]
    distances = [int(''.join(lines[1].split()[1:]))]

    output = part1(times, distances)

    print(output)


def part1(times, distances):
    n = 1
    for t, d in zip(times, distances):
        # want (t - c) * c > d
        # tc - c^2 > d
        # c^2 - tc + d < 0
        # t +- sqrt(t^2 - 4 d) ) / 2
        a = (t - math.sqrt(t ** 2 - 4 * d)) / 2
        b = (t + math.sqrt(t ** 2 - 4 * d)) / 2

        a = math.floor(a)
        b = math.ceil(b)

        n *= (b - a - 1)
    return n


if __name__ == "__main__":
    main()
