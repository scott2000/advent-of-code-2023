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

solveProblem : Grid -> Nat
solveProblem = \grid ->
    when getStart grid is
        Ok start ->
            List.range { start: At 0, end: Length 64 }
            |> List.walk (Set.single start) \allPos, _ -> stepAll allPos grid
            |> Set.len

        Err _ ->
            crash "No start!"

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
