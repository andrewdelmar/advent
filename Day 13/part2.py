import re


def parse_line(line):
    m = re.match(r'.*X.([0-9]+).*Y.([0-9]+)', line)
    return int(m[1]), int(m[2])


def scale(u, v, p):
    ux, uy = u
    vx, vy = v
    px, py = p

    num = vx*py - vy*px
    den = vx*uy - ux*vy

    if den != 0 and num % den != 0:
        return False, 0
    else:
        return True, num // den


def tokens_needed(a, b, p):
    px, py = p
    p = (px + 10000000000000, py + 10000000000000)

    ea, sa = scale(a, b, p)
    eb, sb = scale(b, a, p)

    # Assume that p is linearly independent or both a and b.
    # And, that a and b are independent of each other.
    if ea and eb and sa >= 0 and sb >= 0:
        return 3*sa + sb

    return 0


with open('input.txt') as input:
    lines = input.readlines()
    total = 0
    for i in range(0, len(lines), 4):
        al, bl, pl, *_ = lines[i:]
        a = parse_line(al)
        b = parse_line(bl)
        p = parse_line(pl)
        total += tokens_needed(a, b, p)

    print("Tokens needed:", total)
