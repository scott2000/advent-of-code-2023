app "advent-of-code"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.6.2/c7T4Hp8bAdWz3r9ZrhboBzibCjJag8d0IP_ljb42yVc.tar.br",
        parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.3.0/-e3ebWWmlFPfe9fYrr2z1urfslzygbtQQsl69iH1qzQ.tar.br",
    }
    imports [
        pf.Stdout,
        pf.Task.{ Task },
        parser.Core.{ const, keep, skip, oneOf, oneOrMore, chompUntil },
        parser.String.{ Utf8, parseStr, codeunit, digits },
        "../../inputs/day18.txt" as inputFile : Str,
    ]
    provides [main] to pf

Parser a : Core.Parser Utf8 a

Direction : [
    North,
    South,
    West,
    East,
]

Operation : {
    dir : Direction,
    count : I64,
}

main : Task {} *
main =
    when parseStr problemParser inputFile is
        Ok problem ->
            problem
            |> getPoints
            |> getArea
            |> Num.toStr
            |> Stdout.line

        Err (ParsingIncomplete rest) ->
            Stdout.line "Parsing incomplete: \(rest)"

        Err (ParsingFailure error) ->
            Stdout.line "Parsing error: \(error)"

problemParser : Parser (List Operation)
problemParser =
    oneOrMore operationParser

operationParser : Parser Operation
operationParser =
    const (\dir -> \count -> { dir, count: Num.toI64 count })
    |> keep directionParser
    |> skip (codeunit ' ')
    |> keep digits
    |> skip (chompUntil '\n')
    |> skip (codeunit '\n')

directionParser : Parser Direction
directionParser =
    oneOf [
        const North |> skip (codeunit 'U'),
        const South |> skip (codeunit 'D'),
        const West |> skip (codeunit 'L'),
        const East |> skip (codeunit 'R'),
    ]

Pos : (I64, I64)

getArea : List Pos -> I64
getArea = \vertices ->
    interior = getInteriorArea vertices
    edgeLength = getEdgeLength vertices

    interior + edgeLength // 2 + 1

getInteriorArea : List Pos -> I64
getInteriorArea = \vertices ->
    vertices
    |> List.map2 (List.dropFirst vertices 1) \(x1, y1), (x2, y2) ->
        x1 * y2 - x2 * y1
    |> List.sum
    |> Num.abs
    |> Num.divTrunc 2

getEdgeLength : List Pos -> I64
getEdgeLength = \vertices ->
    vertices
    |> List.map2 (List.dropFirst vertices 1) \(x1, y1), (x2, y2) ->
        Num.absDiff x1 x2 + Num.absDiff y1 y2
    |> List.sum

getPoints : List Operation -> List Pos
getPoints = \ops ->
    ops
    |> List.walk ([(0, 0)], (0, 0)) \(list, (x, y)), op ->
        next =
            when op.dir is
                North -> (x, y - op.count)
                South -> (x, y + op.count)
                West -> (x - op.count, y)
                East -> (x + op.count, y)

        (List.append list next, next)
    |> .0
