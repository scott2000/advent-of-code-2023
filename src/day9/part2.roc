app "advent-of-code"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.6.2/c7T4Hp8bAdWz3r9ZrhboBzibCjJag8d0IP_ljb42yVc.tar.br",
    }
    imports [
        pf.Stdout,
        pf.Task.{ Task },
        "../../inputs/day9.txt" as inputFile : Str,
    ]
    provides [main] to pf

Seq : List I64

main : Task {} *
main =
    inputFile
    |> Str.split "\n"
    |> List.dropIf Str.isEmpty
    |> List.map \line ->
        line
        |> Str.split " "
        |> List.dropIf Str.isEmpty
        |> List.keepOks Str.toI64
    |> List.map predictNext
    |> List.sum
    |> Num.toStr
    |> Stdout.line

predictNext : Seq -> I64
predictNext = \seq ->
    if List.all seq \num -> num == 0 then
        0
    else
        differenceResult =
            List.walkBackwards seq LastStep \step, num ->
                when step is
                    LastStep ->
                        MiddleStep [] num

                    MiddleStep acc first ->
                        MiddleStep (List.prepend acc (first - num)) num

        when differenceResult is
            LastStep -> 0
            MiddleStep difference first ->
                nextDifference = predictNext difference

                first - nextDifference
