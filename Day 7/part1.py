def has_valid_eq(target, acc, parts):
    if acc == target and not parts:
        return True
    if not parts or acc > target:
        return False

    part, *parts = parts;
    valid_add = has_valid_eq(target, acc + part, parts)
    valid_mul = has_valid_eq(target, acc * part, parts)
    
    return valid_add or valid_mul

with open('input.txt') as input:
    total = 0
    for line in input:
        target, first, *parts = [int(x) for x in line.replace(':', '').split()]
        if has_valid_eq(target, first, parts):
            total += target

    print(total)
        