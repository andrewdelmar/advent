def validPattern(pattern, towels):
    valid = [False]*(len(pattern) + 1)
    valid[len(pattern)] = True

    for i in range(len(pattern) - 1, -1, -1):
        for towel in towels:
            if pattern[i:].startswith(towel) and valid[i + len(towel)]:
                valid[i] = True
                break

    return valid[0]


with open('input.txt') as input:
    lines = input.readlines()
    towels = lines[0].rstrip().split(", ")
    patterns = [l.rstrip() for l in lines[2:]]

    totalValid = 0
    for pattern in patterns:
        if validPattern(pattern, towels):
            totalValid += 1

    print("Total Valid:", totalValid)
