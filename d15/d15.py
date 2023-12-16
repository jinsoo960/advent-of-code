def process(sequences):
    boxes = [[] for _ in range(256)]

    for s in sequences:
        try:
            i = s.index('=')
            label = s[:i]
            n = hash(label)
            focalLength = int(s[i+1:])
            found = False
            for i, (l, _) in enumerate(boxes[n]):
                if l == label:
                    boxes[n][i] = (label, focalLength)
                    found = True
                    break
            if not found:
                boxes[n].append((label, focalLength))

        except ValueError:
            label = s[:-1]
            n = hash(label)
            for i, (l, _) in enumerate(boxes[n]):
                if l == label:
                    boxes[n].pop(i)
                    break
    return boxes


def totalStrength(boxes):
    n = 0
    for i, box in enumerate(boxes):
        for j, (_, k) in enumerate(box):
            n += k * (1 + j) * (1 + i)
    return n


def hash(s):
    n = 0
    for c in s:
        n = (n + ord(c)) * 17 % 256
    return n


def main():
    filename = "input"
    with open(filename) as f:
        file = f.readline().strip()

    sequences = file.split(',')

    print(sum(map(hash, sequences)))

    boxes = process(sequences)
    strength = totalStrength(boxes)
    print(strength)


if __name__ == "__main__":
    main()
