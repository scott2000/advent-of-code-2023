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
        "../../inputs/day16.txt" as inputFile : Str,
    ]
    provides [main] to pf

Parser a : Core.Parser Utf8 a

Tile : [
    Empty,
    Splitter [Vert, Horiz],
    Mirror [Forward, Backward],
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
        const Empty |> skip (codeunit '.'),
        const (Splitter Vert) |> skip (codeunit '|'),
        const (Splitter Horiz) |> skip (codeunit '-'),
        const (Mirror Forward) |> skip (codeunit '/'),
        const (Mirror Backward) |> skip (codeunit '\\'),
    ]

Direction : [North, East, South, West]

Laser : {
    row : I32,
    col : I32,
    dir : Direction,
}

State : {
    seen : Set Laser,
    active : Set Laser,
}

getTile : Laser, Grid -> Result Tile [NotFound]
getTile = \laser, grid ->
    List.get grid (Num.toNat laser.row)
    |> Result.try \row -> List.get row (Num.toNat laser.col)
    |> Result.mapErr \_ -> NotFound

solveProblem : Grid -> Nat
solveProblem = \grid ->
    firstLaser = {
        row: 0,
        col: 0,
        dir: East,
    }

    initialState = {
        seen: Set.single firstLaser,
        active: Set.single firstLaser,
    }

    finalState = stepUntilDone initialState grid

    finalState.seen
    |> Set.map \{ row, col } -> (row, col)
    |> Set.len

stepUntilDone : State, Grid -> State
stepUntilDone = \state, grid ->
    if Set.isEmpty state.active then
        state
    else
        stepState state grid
        |> stepUntilDone grid

stepState : State, Grid -> State
stepState = \state, grid ->
    active =
        state.active
        |> Set.toList
        |> List.joinMap \laser -> stepLaser laser grid
        |> List.keepIf \laser -> getTile laser grid |> Result.isOk
        |> Set.fromList
        |> Set.difference state.seen

    seen = Set.union state.seen active

    { seen, active }

stepLaser : Laser, Grid -> List Laser
stepLaser = \laser, grid ->
    when getTile laser grid is
        Ok (Splitter Vert) if laser.dir == West || laser.dir == East ->
            [
                getNext { laser & dir: North },
                getNext { laser & dir: South },
            ]

        Ok (Splitter Horiz) if laser.dir == North || laser.dir == South ->
            [
                getNext { laser & dir: West },
                getNext { laser & dir: East },
            ]

        Ok (Mirror Forward) ->
            dir =
                when laser.dir is
                    North -> East
                    East -> North
                    South -> West
                    West -> South

            [getNext { laser & dir }]

        Ok (Mirror Backward) ->
            dir =
                when laser.dir is
                    North -> West
                    East -> South
                    South -> East
                    West -> North

            [getNext { laser & dir }]

        Ok _ ->
            [getNext laser]

        Err _ ->
            []

getNext : Laser -> Laser
getNext = \laser ->
    when laser.dir is
        North -> { laser & row: laser.row - 1 }
        East -> { laser & col: laser.col + 1 }
        South -> { laser & row: laser.row + 1 }
        West -> { laser & col: laser.col - 1 }
