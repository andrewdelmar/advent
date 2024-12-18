def add_c(a, b):
    ax, ay = a
    bx, by = b
    return (ax + bx, ay + by)


def sub_c(a, b):
    ax, ay = a
    bx, by = b
    return (ax - bx, ay - by)


plots = {}

with open('input.txt') as input:
    for y, line in enumerate(input):
        for x, char in enumerate(line):
            if char != '\n':
                plots[(x, y)] = char


o_dirs = [(-1, 0), (1, 0), (0, -1), (0, 1)]
f_dirs = [(0, -1), (0, -1), (-1, 0), (-1, 0)]


def build_fences(coord, visited, fences):
    visited.add(coord)
    p_type = plots[coord]

    for o_dir, f_dir in zip(o_dirs, f_dirs):
        o_coord = add_c(o_dir, coord)
        if o_coord in plots and plots[o_coord] == p_type:
            if o_coord not in visited:
                build_fences(o_coord, visited, fences)
        else:
            topleft = follow_fence(p_type, coord, o_coord, f_dir, fences)
            fences.add(topleft)


def follow_fence(p_type, in_c, out_c, f_dir, fences):
    while in_c in plots and plots[in_c] == p_type and (out_c not in plots or plots[out_c] != p_type):
        in_c = add_c(in_c, f_dir)
        out_c = add_c(out_c, f_dir)

    in_c = sub_c(in_c, f_dir)
    out_c = sub_c(out_c, f_dir)

    return add_c(in_c, out_c)


total_cost = 0
all_visited = set()
for coord, plot in plots.items():
    if coord not in all_visited:
        plot_visited = set()
        fences = set()
        build_fences(coord, plot_visited, fences)

        total_cost += len(fences) * len(plot_visited)
        all_visited |= plot_visited

print("Total Cost:", total_cost)
