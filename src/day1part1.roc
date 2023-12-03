app "advent-of-code"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.6.2/c7T4Hp8bAdWz3r9ZrhboBzibCjJag8d0IP_ljb42yVc.tar.br" }
    imports [
        pf.Stdout,
        pf.Task.{ Task },
        "../inputs/day1.txt" as inputFile : Str,
    ]
    provides [main] to pf

main : Task {} *
main =
    inputFile
    |> solution
    |> Num.toStr
    |> Stdout.line

solution : Str -> Nat
solution = \input ->
    input
    |> Str.split "\n"
    |> List.dropIf Str.isEmpty
    |> List.keepOks parseNum
    |> List.sum

parseNum : Str -> Result Nat [ListWasEmpty]
parseNum = \line ->
    digits =
        line
        |> Str.graphemes
        |> List.keepOks Str.toNat

    first <- List.first digits |> Result.try
    last <- List.last digits |> Result.try
    Ok (first * 10 + last)

