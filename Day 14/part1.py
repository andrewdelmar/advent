import re


def move(p, v, step):
    px, py = p
    vx, vy = v

    return ((px + vx*step) % 101, (py + vy*step) % 103)


def quadrant(p):
    px, py = p

    hx, hy = 101//2, 103//2

    if px < hx and py < hy:
        return 'A'
    elif px > hx and py < hy:
        return 'B'
    elif px > hx and py > hy:
        return 'C'
    elif px < hx and py > hy:
        return 'D'

    return 'Middle'


with open('input.txt') as input:
    quad_c = {}

    for line in input:
        m = re.match(r'p=(-?[0-9]*),(-?[0-9]*) v=(-?[0-9]*),(-?[0-9]*)', line)
        p = (int(m[1]), int(m[2]))
        v = (int(m[3]), int(m[4]))

        q = quadrant(move(p, v, 100))
        quad_c[q] = quad_c.get(q, 0) + 1

    safety = quad_c.get('A', 0) * quad_c.get('B', 0) * \
        quad_c.get('C', 0) * quad_c.get('D', 0)

    print("Safety factor:", safety)
