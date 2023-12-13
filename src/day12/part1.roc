app "advent-of-code"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.6.2/c7T4Hp8bAdWz3r9ZrhboBzibCjJag8d0IP_ljb42yVc.tar.br",
        parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.3.0/-e3ebWWmlFPfe9fYrr2z1urfslzygbtQQsl69iH1qzQ.tar.br",
    }
    imports [
        pf.Stdout,
        pf.Task.{ Task },
        parser.Core.{ const, skip, keep, many, oneOf, sepBy },
        parser.String.{ Utf8, parseStr, codeunit, digits },
        "../../inputs/day12.txt" as inputFile : Str,
    ]
    provides [main] to pf

Parser a : Core.Parser Utf8 a

SpringStatus : [
    Operational,
    Damaged,
]

Spring : Result SpringStatus [Unknown]

Row : {
    springs : List Spring,
    groups : List Nat,
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

        Err (ParsingFailure _) ->
            Stdout.line "Parsing error"

problemParser : Parser (List Row)
problemParser =
    many rowParser

rowParser : Parser Row
rowParser =
    const (\springs -> \groups -> { springs, groups })
    |> keep (many springParser)
    |> skip (codeunit ' ')
    |> keep (sepBy digits (codeunit ','))
    |> skip (codeunit '\n')

springParser : Parser Spring
springParser =
    oneOf [
        const (Ok Operational) |> skip (codeunit '.'),
        const (Ok Damaged) |> skip (codeunit '#'),
        const (Err Unknown) |> skip (codeunit '?'),
    ]

solveProblem : List Row -> Nat
solveProblem = \rows ->
    rows
    |> List.map \row -> countArrangements row.springs NotInGroup row.groups
    |> List.sum

countArrangements : List Spring, [InGroup Nat, NotInGroup], List Nat -> Nat
countArrangements = \springs, leftInGroup, groups ->
    when List.first springs is
        Err ListWasEmpty ->
            when (leftInGroup, groups) is
                (InGroup 0, []) -> 1
                (NotInGroup, []) -> 1
                _ -> 0

        Ok spring ->
            rest = List.dropFirst springs 1

            getPossibleStatuses spring
            |> List.map \status ->
                when status is
                    Operational ->
                        when leftInGroup is
                            InGroup n if n > 0 ->
                                0

                            _ ->
                                countArrangements rest NotInGroup groups

                    Damaged ->
                        when leftInGroup is
                            InGroup 0 ->
                                0

                            InGroup n ->
                                countArrangements rest (InGroup (n - 1)) groups

                            NotInGroup ->
                                when List.first groups is
                                    Ok group ->
                                        countArrangements rest (InGroup (group - 1)) (List.dropFirst groups 1)

                                    Err ListWasEmpty ->
                                        0
            |> List.sum

getPossibleStatuses : Spring -> List SpringStatus
getPossibleStatuses = \spring ->
    when spring is
        Ok status -> [status]
        Err Unknown -> [Operational, Damaged]
