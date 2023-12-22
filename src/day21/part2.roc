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
        "../../inputs/day21.txt" as inputFile : Str,
    ]
    provides [main] to pf

Parser a : Core.Parser Utf8 a

Tile : [
    Empty,
    Rock,
    Start,
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
        const Rock |> skip (codeunit '#'),
        const Start |> skip (codeunit 'S'),
    ]

Pos : (I32, I32)

getTile : Pos, Grid -> Result Tile [NotFound]
getTile = \(i, j), grid ->
    List.get grid (Num.toNat i)
    |> Result.try \row -> List.get row (Num.toNat j)
    |> Result.mapErr \_ -> NotFound

size : Nat
size = 131

evenStepCount : Nat
evenStepCount = 140

oddStepCount : Nat
oddStepCount = 141

steps : Nat
steps = 26501365

solveProblem : Grid -> Nat
solveProblem = \grid ->
    when getStart grid is
        Ok start ->
            left = (Num.toI32 size // 2, 0)
            right = (Num.toI32 size // 2, Num.toI32 size - 1)
            top = (0, Num.toI32 size // 2)
            bottom = (Num.toI32 size // 2, Num.toI32 size - 1)

            topLeft = (0, 0)
            topRight = (0, Num.toI32 size - 1)
            bottomLeft = (Num.toI32 size - 1, 0)
            bottomRight = (Num.toI32 size - 1, Num.toI32 size - 1)

            stepsAfterFirstEdgeReached = steps - size // 2
            stepsAfterFirstCornerReached = stepsAfterFirstEdgeReached - (size // 2)

            radius = stepsAfterFirstCornerReached // size
            remainder = stepsAfterFirstCornerReached % size
            cornerRemainder = remainder + (size // 2)
            innerEdgeRemainder = remainder + size - 1
            outerEdgeRemainder = Num.subSaturated remainder 1

            #       K E L
            #     K G B H L
            #   K G B A B H L
            # K G B A B A B H L
            # C B A B A B A B D
            # M I B A B A B J N
            #   M I B A B J N
            #     M I B J N
            #       M F N

            # A, B
            odd = countAfterStepsAndFirst grid [start] (oddStepCount + 1)
            even = countAfterStepsAndFirst grid [start] (evenStepCount + 1)

            # C, D, E, F
            topGrid = countAfterStepsAndFirst grid [top] cornerRemainder
            bottomGrid = countAfterStepsAndFirst grid [bottom] cornerRemainder
            leftGrid = countAfterStepsAndFirst grid [left] cornerRemainder
            rightGrid = countAfterStepsAndFirst grid [right] cornerRemainder

            # G, H, I, J
            innerEdgeGrids =
                topLeftGrid = countAfterStepsAndFirst grid [topLeft] innerEdgeRemainder
                topRightGrid = countAfterStepsAndFirst grid [topRight] innerEdgeRemainder
                bottomLeftGrid = countAfterStepsAndFirst grid [bottomLeft] innerEdgeRemainder
                bottomRightGrid = countAfterStepsAndFirst grid [bottomRight] innerEdgeRemainder

                topLeftGrid + topRightGrid + bottomLeftGrid + bottomRightGrid

            # K, L, M, N
            outerEdgeGrids =
                topLeftGrid = countAfterStepsAndFirst grid [topLeft] outerEdgeRemainder
                topRightGrid = countAfterStepsAndFirst grid [topRight] outerEdgeRemainder
                bottomLeftGrid = countAfterStepsAndFirst grid [bottomLeft] outerEdgeRemainder
                bottomRightGrid = countAfterStepsAndFirst grid [bottomRight] outerEdgeRemainder

                topLeftGrid + topRightGrid + bottomLeftGrid + bottomRightGrid

            axes = odd + 4 * (even * ((radius + 1) // 2) + odd * (radius // 2))

            diagonals =
                List.range { start: At 1, end: Before radius }
                |> List.map \i ->
                    if Num.isEven i then
                        i * even
                    else
                        i * odd
                |> List.sum
                |> Num.mul 4

            inside = axes + diagonals
            edges = radius * innerEdgeGrids + (radius + 1) * outerEdgeGrids
            corners = topGrid + bottomGrid + leftGrid + rightGrid

            inside + edges + corners

        Err _ ->
            crash "No start!"

countAfterStepsAndFirst : Grid, List Pos, Nat -> Nat
countAfterStepsAndFirst = \grid, start, count ->
    if count == 0 then
        0
    else
        List.range { start: At 0, end: Length (count - 1) }
        |> List.walk (Set.fromList start) \allPos, _ -> stepAll allPos grid
        |> Set.len

getStart : Grid -> Result Pos [NotFound]
getStart = \grid ->
    List.walkWithIndex grid (Err NotFound) \result, row, i ->
        List.walkWithIndex row result \result2, val, j ->
            if val == Start then
                Ok (Num.toI32 i, Num.toI32 j)
            else
                result2

stepAll : Set Pos, Grid -> Set Pos
stepAll = \allPos, grid ->
    Set.joinMap allPos \pos -> step pos grid

step : Pos, Grid -> Set Pos
step = \(i, j), grid ->
    [(i - 1, j), (i + 1, j), (i, j - 1), (i, j + 1)]
    |> List.keepIf \pos ->
        when getTile pos grid is
            Ok Empty -> Bool.true
            Ok Start -> Bool.true
            _ -> Bool.false
    |> Set.fromList
