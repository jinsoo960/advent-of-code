from copy import deepcopy


class Part:
    def __init__(self, s):
        s = s[1:-1]
        cats = s.split(',')

        self.x = int(cats[0][2:])
        self.m = int(cats[1][2:])
        self.a = int(cats[2][2:])
        self.s = int(cats[3][2:])


class PartInterval:
    def __init__(self, x, m, a, s):
        self.x = x
        self.m = m
        self.a = a
        self.s = s

    def isEmpty(self):
        return self.x[0] > self.x[1] or self.m[0] > self.m[1] \
            or self.a[0] > self.a[1] or self.s[0] > self.s[1]

    def size(self):
        return (self.x[1] - self.x[0] + 1) * (self.m[1] - self.m[0] + 1) \
                * (self.a[1] - self.a[0] + 1) * (self.s[1] - self.s[0] + 1)

    def __repr__(self):
        return f"(x: {self.x}, m: {self.m}, a: {self.a}, s: {self.s})"


class R:
    def __init__(self, category, comp, n, result):
        self.category = category
        self.n = n
        self.result = result
        self.comp = comp

        if category is None:
            self.category = None
            self.f = lambda _: True
            return

        if comp == '>':
            self.f = lambda part: getattr(part, self.category) > self.n
        else:
            self.f = lambda part: getattr(part, self.category) < self.n

    def apply(self, part):
        return self.f(part)

    # return pair of evaluating to True and False
    def applyInterval(self, partInt):
        if self.category is None:
            return partInt, PartInterval((-1, 0), (-1, 0), (-1, 0), (-1, 0))
        relevant = getattr(partInt, self.category)
        p1 = deepcopy(partInt)
        p2 = deepcopy(partInt)
        if self.comp == '>':
            setattr(p1, self.category, (self.n + 1, relevant[1]))
            setattr(p2, self.category, (relevant[0], self.n))
        else:
            setattr(p1, self.category, (relevant[0], self.n - 1))
            setattr(p2, self.category, (self.n, relevant[1]))
        return p1, p2


class Rule:
    def __init__(self, s):
        i = s.index('{')
        self.name = s[:i]
        rs = s[i+1:-1].split(',')
        self.rules = []

        for r in rs[:-1]:
            tmp = r.split(':')

            cond = tmp[0]
            res = tmp[1]

            cat = cond[0]
            comp = cond[1]
            n = int(cond[2:])

            self.rules.append(R(cat, comp, n, res))
        self.rules.append(R(None, None, None, rs[-1]))

    def applyRule(self, part):
        for r in self.rules:
            if r.apply(part):
                return r.result
        raise

    def applyRuleInt(self, partInt):
        p2 = partInt
        results = []
        for r in self.rules:
            p1, p2 = r.applyInterval(p2)
            if not p1.isEmpty():
                results.append((p1, r.result))
            if p2.isEmpty():
                break
        return results


def applyRules(rules, part):
    res = rules["in"].applyRule(part)
    while res != 'A' and res != 'R':
        res = rules[res].applyRule(part)

    return res


def applyRulesInterval(rules):
    interval = (1, 4000)
    initial = PartInterval(interval, interval, interval, interval)

    toApply = [(initial, "in")]

    count = 0

    while toApply:
        current = toApply.pop()

        results = rules[current[1]].applyRuleInt(current[0])

        for res in results:
            if res[1] == 'A':
                count += res[0].size()
            elif res[1] == 'R':
                continue
            elif not res[0].isEmpty():
                toApply.append(res)

    return count


def getRating(rules, parts):
    rating = 0
    i = 0
    for p in parts:
        if applyRules(rules, p) == 'A':
            rating += p.x + p.m + p.a + p.s
        i += 1
    return rating


def main():
    filename = "input"
    with open(filename) as f:
        file = [line.strip() for line in f.readlines()]

    i = file.index("")

    parts = file[i+1:]
    parts = list(map(Part, parts))

    rules = file[:i]
    rules = list(map(Rule, rules))
    rules = {r.name: r for r in rules}

    # rating = getRating(rules, parts)
    # print(rating)

    c = applyRulesInterval(rules)
    print(c)


if __name__ == "__main__":
    main()
