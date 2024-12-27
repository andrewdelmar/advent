def numCombos(pattern, towels):
    combos = [0]*(len(pattern) + 1)
    combos[len(pattern)] = 1

    for i in range(len(pattern) - 1, -1, -1):
        for towel in towels:
            if pattern[i:].startswith(towel):
                combos[i] += combos[i + len(towel)]

    return combos[0]


with open('input.txt') as input:
    lines = input.readlines()
    towels = lines[0].rstrip().split(", ")
    patterns = [l.rstrip() for l in lines[2:]]

    totalCombos = 0
    for pattern in patterns:
        totalCombos += numCombos(pattern, towels)

    print("Total Combos:", totalCombos)
