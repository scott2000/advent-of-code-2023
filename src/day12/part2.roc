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
    |> List.map \{ springs, groups } -> {
        springs: springs
        |> List.repeat 5
        |> List.intersperse [Err Unknown]
        |> List.join,
        groups: groups
        |> List.repeat 5
        |> List.join,
    }
    |> List.map \row ->
        (_, result) = countArrangements (Dict.empty {}) {
            springs: row.springs,
            leftInGroup: NotInGroup,
            groups: row.groups,
        }

        result
    |> List.sum

CountArgs : {
    springs : List Spring,
    leftInGroup : [InGroup Nat, NotInGroup],
    groups : List Nat,
}

CountState : Dict CountArgs Nat

countArrangements : CountState, CountArgs -> (CountState, Nat)
countArrangements = \state, args ->
    when Dict.get state args is
        Ok result ->
            (state, result)

        Err KeyNotFound ->
            (state1, result) = countArrangementsHelper state args
            (Dict.insert state1 args result, result)

countArrangementsHelper : CountState, CountArgs -> (CountState, Nat)
countArrangementsHelper = \state, { springs, leftInGroup, groups } ->
    when List.first springs is
        Err ListWasEmpty ->
            when (leftInGroup, groups) is
                (InGroup 0, []) -> (state, 1)
                (NotInGroup, []) -> (state, 1)
                _ -> (state, 0)

        Ok spring ->
            rest = List.dropFirst springs 1

            getPossibleStatuses spring
            |> List.walk (state, 0) \(state1, acc), status ->
                when status is
                    Operational ->
                        when leftInGroup is
                            InGroup n if n > 0 ->
                                (state1, acc)

                            _ ->
                                (state2, result) = countArrangements state1 {
                                    springs: rest,
                                    leftInGroup: NotInGroup,
                                    groups,
                                }

                                (state2, acc + result)

                    Damaged ->
                        when leftInGroup is
                            InGroup 0 ->
                                (state1, acc)

                            InGroup n ->
                                (state2, result) = countArrangements state1 {
                                    springs: rest,
                                    leftInGroup: InGroup (n - 1),
                                    groups,
                                }

                                (state2, acc + result)

                            NotInGroup ->
                                when List.first groups is
                                    Ok group ->
                                        (state2, result) = countArrangements state1 {
                                            springs: rest,
                                            leftInGroup: InGroup (group - 1),
                                            groups: List.dropFirst groups 1,
                                        }

                                        (state2, acc + result)

                                    Err ListWasEmpty ->
                                        (state1, acc)

getPossibleStatuses : Spring -> List SpringStatus
getPossibleStatuses = \spring ->
    when spring is
        Ok status -> [status]
        Err Unknown -> [Operational, Damaged]
