open System.IO

let input = File.ReadAllLines "input.txt"

let sizeX, sizeY = input.[0].Length, input.Length

let startX, startY =
    let mutable sx, sy = 0, 0

    for y in 0 .. sizeY - 1 do
        for x in 0 .. sizeX - 1 do
            if input.[y].[x] = '^' then
                sx <- x
                sy <- y

    sx, sy

let obstacles =
    let obs x y = input.[y].[x] = '#'

    Array2D.init sizeX sizeY obs

type Dir =
    | North
    | East
    | South
    | West

let dx dir =
    match dir with
    | East -> 1
    | West -> -1
    | _ -> 0

let dy dir =
    match dir with
    | North -> -1
    | South -> 1
    | _ -> 0

let turn dir =
    match dir with
    | North -> East
    | East -> South
    | South -> West
    | West -> North

let isLoop (obstacles: bool array2d) =
    let visited = Array2D.create sizeX sizeY Set.empty
    let mutable x, y = startX, startY
    let mutable dir = North

    let inBounds x y =
        x >= 0 && x < sizeX && y >= 0 && y < sizeY

    let obstacleAhead x y dir =
        let (nx, ny) = x + dx dir, y + dy dir
        inBounds nx ny && obstacles.[nx, ny]

    let mutable loop = false

    while inBounds x y && not loop do
        if visited.[x, y].Contains dir then
            loop <- true

        visited.[x, y] <- visited.[x, y].Add dir

        while obstacleAhead x y dir do
            dir <- turn dir

        x <- x + dx dir
        y <- y + dy dir

    loop

let findLoops =
    let mutable count = 0

    for y in 0 .. sizeY - 1 do
        for x in 0 .. sizeX - 1 do
            let obs = Array2D.copy obstacles
            obs.[x, y] <- true

            if isLoop obs then
                count <- count + 1

    count

printfn "%d" findLoops
