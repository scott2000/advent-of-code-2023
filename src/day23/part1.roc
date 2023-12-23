app "advent-of-code"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.6.2/c7T4Hp8bAdWz3r9ZrhboBzibCjJag8d0IP_ljb42yVc.tar.br",
        parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.3.0/-e3ebWWmlFPfe9fYrr2z1urfslzygbtQQsl69iH1qzQ.tar.br",
    }
    imports [
        pf.Stdout,
        pf.Task.{ Task },
        parser.Core.{ const, skip, oneOrMore, oneOf },
        parser.String.{ Utf8, parseStr, codeunit },
        "../../inputs/day23.txt" as inputFile : Str,
    ]
    provides [main] to pf

Parser a : Core.Parser Utf8 a

Direction : [North, East, South, West]

Tile : [
    Path,
    Forest,
    Slope Direction,
]

Grid : List (List Tile)

main : Task {} *
main =
    when parseStr problemParser inputFile is
        Ok problem ->
            problem
            |> solveProblem
            |> Num.toStr
            |> Stdout.line

        Err (ParsingIncomplete rest) ->
            Stdout.line "Parsing incomplete: \(rest)"

        Err (ParsingFailure error) ->
            Stdout.line "Parsing error: \(error)"

problemParser : Parser Grid
problemParser =
    oneOrMore rowParser

rowParser : Parser (List Tile)
rowParser =
    oneOrMore tileParser
    |> skip (codeunit '\n')

tileParser : Parser Tile
tileParser =
    oneOf [
        const Path |> skip (codeunit '.'),
        const Forest |> skip (codeunit '#'),
        const (Slope North) |> skip (codeunit '^'),
        const (Slope East) |> skip (codeunit '>'),
        const (Slope South) |> skip (codeunit 'v'),
        const (Slope West) |> skip (codeunit '<'),
    ]

Pos : (I32, I32)

Path : {
    seen : Set Pos,
    head : Pos,
}

solveProblem : Grid -> Nat
solveProblem = \grid ->
    when getStart grid is
        Ok start ->
            initialPaths = [
                {
                    seen: Set.empty {},
                    head: start,
                },
            ]

            solveProblemHelper grid initialPaths 0

        Err _ ->
            crash "Start not found!"

solveProblemHelper : Grid, List Path, Nat -> Nat
solveProblemHelper = \grid, paths, longestPath ->
    if List.isEmpty paths then
        longestPath
    else
        newPaths = stepPaths paths grid

        newLongestPath =
            newPaths
            |> List.keepIf \path -> isDone path grid
            |> List.map .seen
            |> List.map Set.len
            |> List.walk longestPath Num.max

        notDonePaths =
            newPaths
            |> List.dropIf \path -> isDone path grid

        solveProblemHelper grid notDonePaths newLongestPath

getStart : Grid -> Result Pos [NotFound]
getStart = \grid ->
    grid
    |> List.get 0
    |> Result.try \row ->
        List.findFirstIndex row \tile -> tile == Path
    |> Result.map \col -> (0, Num.toI32 col)
    |> Result.mapErr \_ -> NotFound

getTile : Pos, Grid -> Result Tile [NotFound]
getTile = \(i, j), grid ->
    List.get grid (Num.toNat i)
    |> Result.try \row -> List.get row (Num.toNat j)
    |> Result.mapErr \_ -> NotFound

isDone : Path, Grid -> Bool
isDone = \path, grid ->
    path.head.0 == Num.toI32 (List.len grid - 1)

stepPaths : List Path, Grid -> List Path
stepPaths = \paths, grid ->
    paths
    |> List.joinMap \path -> stepPath path grid

stepPath : Path, Grid -> List Path
stepPath = \path, grid ->
    (row, col) = path.head

    north = (row - 1, col)
    east = (row, col + 1)
    south = (row + 1, col)
    west = (row, col - 1)

    newHeads =
        when getTile path.head grid is
            Ok Path -> [north, east, south, west]
            Ok (Slope North) -> [north]
            Ok (Slope East) -> [east]
            Ok (Slope South) -> [south]
            Ok (Slope West) -> [west]
            _ -> []

    newHeads
    |> List.keepIf \newHead ->
        getTile newHead grid
        |> Result.map \tile -> tile != Forest
        |> Result.withDefault Bool.false
    |> List.dropIf \newHead ->
        Set.contains path.seen newHead
    |> List.map \newHead -> {
        seen: path.seen |> Set.insert path.head,
        head: newHead,
    }
