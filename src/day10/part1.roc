app "advent-of-code"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.6.2/c7T4Hp8bAdWz3r9ZrhboBzibCjJag8d0IP_ljb42yVc.tar.br",
        parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.3.0/-e3ebWWmlFPfe9fYrr2z1urfslzygbtQQsl69iH1qzQ.tar.br",
    }
    imports [
        pf.Stdout,
        pf.Task.{ Task },
        parser.Core.{ skip, many, oneOf, map },
        parser.String.{ Utf8, parseStr, codeunit },
        "../../inputs/day10.txt" as inputFile : Str,
    ]
    provides [main] to pf

Parser a : Core.Parser Utf8 a

Tile : [
    Vertical,
    Horizontal,
    NorthEast,
    NorthWest,
    SouthWest,
    SouthEast,
    Ground,
    Start,
]

Pos : (I32, I32)

Grid : List (List Tile)

State : {
    steps : Nat,
    seen : Set Pos,
    heads : Set Pos,
}

main : Task {} *
main =
    when parseStr problemParser inputFile is
        Ok problem ->
            solveProblem problem
            |> Num.toStr
            |> Stdout.line

        Err (ParsingIncomplete rest) ->
            Stdout.line "Parsing incomplete: \(rest)"

        Err (ParsingFailure _) ->
            Stdout.line "Parsing error"

problemParser : Parser Grid
problemParser =
    many rowParser

rowParser : Parser (List Tile)
rowParser =
    many tileParser
    |> skip (codeunit '\n')

tileParser : Parser Tile
tileParser =
    oneOf [
        codeunit '|' |> map \_ -> Vertical,
        codeunit '-' |> map \_ -> Horizontal,
        codeunit 'L' |> map \_ -> NorthEast,
        codeunit 'J' |> map \_ -> NorthWest,
        codeunit '7' |> map \_ -> SouthWest,
        codeunit 'F' |> map \_ -> SouthEast,
        codeunit '.' |> map \_ -> Ground,
        codeunit 'S' |> map \_ -> Start,
    ]

solveProblem : Grid -> Nat
solveProblem = \grid ->
    solveProblemHelper grid {
        steps: 0,
        seen: Set.empty {},
        heads: Set.single (getStart grid),
    }

solveProblemHelper : Grid, State -> Nat
solveProblemHelper = \grid, state ->
    heads =
        state.heads
        |> Set.toList
        |> List.joinMap \pos -> getNext grid pos
        |> Set.fromList
        |> Set.difference state.seen

    seen =
        Set.union state.seen heads

    if Set.isEmpty heads then
        state.steps
    else
        solveProblemHelper grid {
            steps: state.steps + 1,
            seen,
            heads,
        }

getStart : Grid -> Pos
getStart = \grid ->
    List.mapWithIndex grid \row, i ->
        List.mapWithIndex row \val, j ->
            when val is
                Start -> Ok (Num.toI32 i, Num.toI32 j)
                _ -> Err NotStart
        |> List.keepOks \x -> x
    |> List.join
    |> List.first
    |> Result.withDefault (0, 0)

getTile : Grid, Pos -> Tile
getTile = \grid, (i, j) ->
    if i < 0 || j < 0 then
        Ground
    else
        List.get grid (Num.toNat i)
        |> Result.try \row -> List.get row (Num.toNat j)
        |> Result.withDefault Ground

getNext : Grid, Pos -> List Pos
getNext = \grid, pos ->
    getPossibleNext grid pos
    |> List.keepIf \newPos ->
        getPossibleNext grid newPos
        |> List.contains pos

getPossibleNext : Grid, Pos -> List Pos
getPossibleNext = \grid, (i, j) ->
    when getTile grid (i, j) is
        Vertical -> [(i - 1, j), (i + 1, j)]
        Horizontal -> [(i, j - 1), (i, j + 1)]
        NorthEast -> [(i - 1, j), (i, j + 1)]
        NorthWest -> [(i - 1, j), (i, j - 1)]
        SouthWest -> [(i + 1, j), (i, j - 1)]
        SouthEast -> [(i + 1, j), (i, j + 1)]
        Ground -> []
        Start ->
            [
                (i - 1, j),
                (i, j - 1),
                (i, j + 1),
                (i + 1, j),
            ]
