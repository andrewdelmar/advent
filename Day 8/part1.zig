const std = @import("std");

const Coord = struct {
    x: i32,
    y: i32,

    fn in_bounds(self: Coord, sx: i32, sy: i32) bool {
        return self.x >= 0 and self.x < sx and self.y >= 0 and self.y < sy;
    }
};

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    var y: i32 = 0;
    var sx: i32 = 0;
    var antennas = std.AutoHashMap(u8, std.ArrayList(Coord)).init(allocator);
    var antinodes = std.AutoHashMap(Coord, void).init(allocator);
    while (try file.reader().readUntilDelimiterOrEofAlloc(allocator, '\n', 10000)) |line| {
        for (line, 0..) |char, x| {
            if (char != '.') {
                const val = try antennas.getOrPut(char);
                if (!val.found_existing) {
                    val.value_ptr.* = std.ArrayList(Coord).init(allocator);
                }

                try val.value_ptr.append(Coord{ .x = @intCast(x), .y = @intCast(y) });
            }
            std.debug.print("{c} {d}\n", .{ char, x });
        }
        sx = @intCast(line.len);
        y += 1;
    }
    const sy = y;

    var ni = antennas.valueIterator();
    while (ni.next()) |coords| {
        for (coords.items, 0..) |a, ai| {
            for (coords.items, 0..) |b, bi| {
                if (ai != bi) {
                    const dx = a.x - b.x;
                    const dy = a.y - b.y;

                    const antinode_a = Coord{ .x = a.x + dx, .y = a.y + dy };
                    if (antinode_a.in_bounds(sx, sy)) {
                        try antinodes.put(antinode_a, {});
                    }

                    const antinode_b = Coord{ .x = b.x - dx, .y = b.y - dy };
                    if (antinode_b.in_bounds(sx, sy)) {
                        try antinodes.put(antinode_b, {});
                    }
                }
            }
        }
    }

    std.debug.print("Antinodes: {d}\n", .{antinodes.count()});
}
