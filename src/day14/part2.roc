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
        "../../inputs/day14.txt" as inputFile : Str,
    ]
    provides [main] to pf

Parser a : Core.Parser Utf8 a

Tile : [Round, Square, Empty]

Grid : List (List Tile)

main : Task {} *
main =
    when parseStr problemParser inputFile is
        Ok problem ->
            problem
            |> transpose
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
        const Round |> skip (codeunit 'O'),
        const Square |> skip (codeunit '#'),
        const Empty |> skip (codeunit '.'),
    ]

getWidth : Grid -> Nat
getWidth = \grid ->
    grid
    |> List.get 0
    |> Result.map List.len
    |> Result.withDefault 0

transpose : Grid -> Grid
transpose = \grid ->
    List.range { start: At 0, end: Length (getWidth grid) }
    |> List.map \col ->
        List.mapTry grid \row ->
            List.get row col
        |> Result.withDefault []

SlideState : {
    seen : List Tile,
    roundCount : Nat,
    emptyCount : Nat,
}

initialState : SlideState
initialState = {
    seen: [],
    roundCount: 0,
    emptyCount: 0,
}

slideRocks : Grid -> Grid
slideRocks = \grid ->
    grid
    |> slideRocksNorth
    |> slideRocksWest
    |> slideRocksSouth
    |> slideRocksEast

slideRocksNorth : Grid -> Grid
slideRocksNorth = \grid ->
    grid
    |> List.map slideRockCol

slideRocksWest : Grid -> Grid
slideRocksWest = \grid ->
    grid
    |> transpose
    |> List.map slideRockCol
    |> transpose

slideRocksSouth : Grid -> Grid
slideRocksSouth = \grid ->
    grid
    |> List.map slideRockColReverse

slideRocksEast : Grid -> Grid
slideRocksEast = \grid ->
    grid
    |> transpose
    |> List.map slideRockColReverse
    |> transpose

slideRockColReverse : List Tile -> List Tile
slideRockColReverse = \col ->
    col
    |> List.reverse
    |> slideRockColHelper initialState
    |> List.reverse

slideRockCol : List Tile -> List Tile
slideRockCol = \col ->
    slideRockColHelper col initialState

slideRockColHelper : List Tile, SlideState -> List Tile
slideRockColHelper = \col, state ->
    when col is
        [] ->
            finishSegment state |> .seen

        [Round, .. as rest] ->
            slideRockColHelper rest { state & roundCount: state.roundCount + 1 }

        [Empty, .. as rest] ->
            slideRockColHelper rest { state & emptyCount: state.emptyCount + 1 }

        [Square, .. as rest] ->
            state1 = finishSegment state
            slideRockColHelper rest { state1 & seen: List.append state1.seen Square }

finishSegment : SlideState -> SlideState
finishSegment = \state ->
    { initialState &
        seen: state.seen
        |> List.concat (List.repeat Round state.roundCount)
        |> List.concat (List.repeat Empty state.emptyCount),
    }

solveProblem : Grid -> Nat
solveProblem = \grid ->
    solveProblemHelper grid (Dict.empty {}) 0 1_000_000_000
    |> List.map scoreColumn
    |> List.sum

solveProblemHelper : Grid, Dict Grid Nat, Nat, Nat -> Grid
solveProblemHelper = \grid, dict, index, endIndex ->
    if index == endIndex then
        grid
    else
        when Dict.get dict grid is
            Ok lastIndex ->
                cycleLength = index - lastIndex

                if cycleLength == 0 then
                    grid
                else
                    left = (endIndex - index) % cycleLength

                    repeatSlide grid left

            Err _ ->
                newDict = Dict.insert dict grid index

                solveProblemHelper (slideRocks grid) newDict (index + 1) endIndex

repeatSlide : Grid, Nat -> Grid
repeatSlide = \grid, n ->
    if n == 0 then
        grid
    else
        repeatSlide (slideRocks grid) (n - 1)

scoreColumn : List Tile -> Nat
scoreColumn = \col ->
    col
    |> List.reverse
    |> List.mapWithIndex \tile, index ->
        when tile is
            Round -> index + 1
            _ -> 0
    |> List.sum
