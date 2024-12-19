import re
import time


def step(b):
    p, v = b
    px, py = p
    vx, vy = v
    np = ((px + vx) % 101, (py + vy) % 103)
    return (np, v)


bots = []
with open('input.txt') as input:
    for line in input:
        m = re.match(r'p=(-?[0-9]*),(-?[0-9]*) v=(-?[0-9]*),(-?[0-9]*)', line)
        p = (int(m[1]), int(m[2]))
        v = (int(m[3]), int(m[4]))

        bots.append((p, v))

t = 0
while True:
    # Dumb leap of faith:
    # Assume they generated the data at whatever time the image appears and
    # never put more than one bot in any cell.
    # This is easier than looking for some sort of structure.
    all_unique = True
    pos = set()
    for (p, _) in bots:
        if p in pos:
            all_unique = False
        pos.add(p)

    if all_unique:
        time.sleep(1)
        for y in range(0, 103):
            for x in range(0, 101):
                if (x, y) in pos:
                    print('##', end='')
                else:
                    print('  ', end='')
            print()

        print("Time:", t)

    bots = list(map(step, bots))
    t += 1
