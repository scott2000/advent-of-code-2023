app "advent-of-code"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.6.2/c7T4Hp8bAdWz3r9ZrhboBzibCjJag8d0IP_ljb42yVc.tar.br",
        parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.3.0/-e3ebWWmlFPfe9fYrr2z1urfslzygbtQQsl69iH1qzQ.tar.br",
    }
    imports [
        pf.Stdout,
        pf.Task.{ Task },
        parser.Core.{ const, keep, skip, ignore, many, sepBy, chompUntil, map2 },
        parser.String.{ Utf8, parseStr, codeunit, digits },
        "../../inputs/day6.txt" as inputFile : Str,
    ]
    provides [main] to pf

Parser a : Core.Parser Utf8 a

Race : {
    time : F64,
    distance : F64,
}

main : Task {} *
main =
    when parseStr problemParser inputFile is
        Ok problem ->
            problem
            |> List.map numWaysToWin
            |> List.product
            |> Num.toStr
            |> Stdout.line

        Err _ ->
            Stdout.line "Parsing error"

problemParser : Parser (List Race)
problemParser =
    times, distances <- map2 inputLine inputLine
    time, distance <- List.map2 times distances
    { time, distance }

inputLine : Parser (List F64)
inputLine =
    const (\nums -> List.map nums Num.toF64)
    |> skip (chompUntil ':')
    |> skip (codeunit ':')
    |> skip spaces
    |> keep (sepBy digits spaces)
    |> skip (codeunit '\n')

spaces : Parser {}
spaces = ignore (many (codeunit ' '))

numWaysToWin : Race -> Nat
numWaysToWin = \{ time, distance } ->
    { low, high } = solveQuadratic -1 time -distance

    Num.floor high - Num.ceiling low + 1

solveQuadratic : F64, F64, F64 -> { low : F64, high : F64 }
solveQuadratic = \a, b, c ->
    root = Num.sqrt (b * b - 4 * a * c)

    minus =
        (-b - root) / (2 * a)

    plus =
        (-b + root) / (2 * a)

    if minus <= plus then
        { low: minus, high: plus }
    else
        { low: plus, high: minus }
