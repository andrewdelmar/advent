def add_c(a, b):
    ax, ay = a
    bx, by = b
    return (ax + bx, ay + by)


plots = {}

with open('input.txt') as input:
    for y, line in enumerate(input):
        for x, char in enumerate(line):
            if char != '\n':
                plots[(x, y)] = char


o_dirs = [(-1, 0), (1, 0), (0, -1), (0, 1)]


def build_fences(coord, visited, fences):
    visited.add(coord)
    p_type = plots[coord]

    for o_dir in o_dirs:
        o_coord = add_c(o_dir, coord)
        if o_coord in plots and plots[o_coord] == p_type:
            if o_coord not in visited:
                build_fences(o_coord, visited, fences)
        else:
            fences.add(add_c(coord, o_coord))


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
