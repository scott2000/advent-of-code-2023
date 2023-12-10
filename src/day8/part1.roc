app "advent-of-code"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.6.2/c7T4Hp8bAdWz3r9ZrhboBzibCjJag8d0IP_ljb42yVc.tar.br",
        parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.3.0/-e3ebWWmlFPfe9fYrr2z1urfslzygbtQQsl69iH1qzQ.tar.br",
    }
    imports [
        pf.Stdout,
        pf.Task.{ Task },
        parser.Core.{ const, keep, skip, ignore, many, oneOf, map, chompWhile },
        parser.String.{ Utf8, parseStr, codeunit, strFromUtf8 },
        "../../inputs/day8.txt" as inputFile : Str,
    ]
    provides [main] to pf

Parser a : Core.Parser Utf8 a

Direction : [Left, Right]

Adjacent : {
    left : Str,
    right : Str,
}

Problem : {
    directions : List Direction,
    graph : Dict Str Adjacent,
}

main : Task {} *
main =
    when parseStr problemParser inputFile is
        Ok problem ->
            solveProblem problem
            |> Num.toStr
            |> Stdout.line

        Err _ ->
            Stdout.line "Parsing error"

problemParser : Parser Problem
problemParser =
    const (\directions -> \entries -> { directions, graph: Dict.fromList entries })
    |> keep (many directionParser)
    |> skip (many (codeunit '\n'))
    |> keep (many entryParser)

directionParser : Parser Direction
directionParser =
    oneOf [
        codeunit 'L' |> map \_ -> Left,
        codeunit 'R' |> map \_ -> Right,
    ]

entryParser : Parser (Str, Adjacent)
entryParser =
    const (\str -> \left -> \right -> (str, { left, right }))
    |> keep strParser
    |> skip spaces
    |> skip (codeunit '=')
    |> skip spaces
    |> skip (codeunit '(')
    |> keep strParser
    |> skip (codeunit ',')
    |> skip spaces
    |> keep strParser
    |> skip (codeunit ')')
    |> skip (codeunit '\n')

strParser : Parser Str
strParser =
    chompWhile \ch -> ch >= 'A' && ch <= 'Z'
    |> map strFromUtf8

spaces : Parser {}
spaces = ignore (many (codeunit ' '))

solveProblem : Problem -> Nat
solveProblem = \problem ->
    solveProblemHelper ("AAA", 0) problem

solveProblemHelper : (Str, Nat), Problem -> Nat
solveProblemHelper = \(str, count), problem ->
    if str == "ZZZ" then
        count
    else
        stepAll (str, count) problem
        |> solveProblemHelper problem

stepAll : (Str, Nat), Problem -> (Str, Nat)
stepAll = \str, problem ->
    List.walk problem.directions str (step problem.graph)

step : Dict Str Adjacent -> ((Str, Nat), Direction -> (Str, Nat))
step = \dict ->
    \(str, count), dir ->
        if str == "ZZZ" then
            (str, count)
        else
            next =
                Dict.get dict str
                |> Result.map \adj -> getAdjacent adj dir
                |> Result.withDefault str

            (next, count + 1)

getAdjacent : Adjacent, Direction -> Str
getAdjacent = \adj, dir ->
    when dir is
        Left -> adj.left
        Right -> adj.right
