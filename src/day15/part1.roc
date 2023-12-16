app "advent-of-code"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.6.2/c7T4Hp8bAdWz3r9ZrhboBzibCjJag8d0IP_ljb42yVc.tar.br",
    }
    imports [
        pf.Stdout,
        pf.Task.{ Task },
        "../../inputs/day15.txt" as inputFile : Str,
    ]
    provides [main] to pf

main : Task {} *
main =
    inputFile
    |> Str.split ","
    |> List.map Str.trim
    |> List.dropIf Str.isEmpty
    |> List.map \str -> hash (Str.toUtf8 str) 0
    |> List.sum
    |> Num.toStr
    |> Stdout.line

hash : List U8, Nat -> Nat
hash = \str, acc ->
    when str is
        [] ->
            acc

        [head, .. as tail] ->
            hash tail ((acc + Num.toNat head) * 17 % 256)
