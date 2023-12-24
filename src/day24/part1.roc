app "advent-of-code"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.6.2/c7T4Hp8bAdWz3r9ZrhboBzibCjJag8d0IP_ljb42yVc.tar.br",
        parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.3.0/-e3ebWWmlFPfe9fYrr2z1urfslzygbtQQsl69iH1qzQ.tar.br",
    }
    imports [
        pf.Stdout,
        pf.Task.{ Task },
        parser.Core.{ const, skip, keep, maybe, oneOrMore, ignore },
        parser.String.{ Utf8, parseStr, codeunit, digits },
        "../../inputs/day24.txt" as inputFile : Str,
    ]
    provides [main] to pf

Parser a : Core.Parser Utf8 a

Vec : {
    x : Dec,
    y : Dec,
    z : Dec,
}

Hailstone : {
    position : Vec,
    velocity : Vec,
}

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

problemParser : Parser (List Hailstone)
problemParser =
    oneOrMore hailstoneParser

hailstoneParser : Parser Hailstone
hailstoneParser =
    const \position -> \velocity -> { position, velocity }
    |> keep vecParser
    |> skip spaces
    |> skip (codeunit '@')
    |> skip spaces
    |> keep vecParser
    |> skip (codeunit '\n')

vecParser : Parser Vec
vecParser =
    const \x -> \y -> \z -> { x, y, z }
    |> keep intParser
    |> skip (codeunit ',')
    |> skip spaces
    |> keep intParser
    |> skip (codeunit ',')
    |> skip spaces
    |> keep intParser

intParser : Parser Dec
intParser =
    const \neg -> \val ->
            if Result.isOk neg then
                Num.toFrac -(Num.toI64 val)
            else
                Num.toFrac val
    |> keep (maybe (codeunit '-'))
    |> keep digits

spaces : Parser {}
spaces = ignore (oneOrMore (codeunit ' '))

solveProblem : List Hailstone -> Nat
solveProblem = \hailstones ->
    List.walkWithIndex hailstones 0 \outer, a, i ->
        List.walkFrom hailstones (i + 1) outer \inner, b ->
            if Result.isOk (intersectionPoint a b) then
                inner + 1
            else
                inner

intersectionPoint : Hailstone, Hailstone -> Result (Dec, Dec) [OutOfRange]
intersectionPoint = \a, b ->
    numer = a.position.x * a.velocity.y / a.velocity.x - b.position.x * b.velocity.y / b.velocity.x - a.position.y + b.position.y
    denom = a.velocity.y / a.velocity.x - b.velocity.y / b.velocity.x

    x = numer / denom
    ay = evaluate a x
    by = evaluate b x

    if isInRange x && (isInRange ay || isInRange by) then
        if isNotBackward a x && isNotBackward b x then
            Ok (x, by + (ay - by) / 2)
        else
            Err OutOfRange
    else
        Err OutOfRange

evaluate : Hailstone, Dec -> Dec
evaluate = \hailstone, x ->
    (x - hailstone.position.x) * hailstone.velocity.y / hailstone.velocity.x + hailstone.position.y

isNotBackward : Hailstone, Dec -> Bool
isNotBackward = \hailstone, x ->
    (x - hailstone.position.x) / hailstone.velocity.x >= -0.000001

isInRange : Dec -> Bool
isInRange = \dec ->
    200000000000000 <= dec && dec <= 400000000000000
