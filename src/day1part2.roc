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
    first <- findFirst line |> Result.try
    last <- findLast line |> Result.try
    Ok (first * 10 + last)

digitNames : List (Str, Nat)
digitNames = [
    ("1", 1),
    ("2", 2),
    ("3", 3),
    ("4", 4),
    ("5", 5),
    ("6", 6),
    ("7", 7),
    ("8", 8),
    ("9", 9),
    ("one", 1),
    ("two", 2),
    ("three", 3),
    ("four", 4),
    ("five", 5),
    ("six", 6),
    ("seven", 7),
    ("eight", 8),
    ("nine", 9),
]

findFirst : Str -> Result Nat [ListWasEmpty]
findFirst = \line ->
    digitNames
    |> List.keepOks (firstIndexOf line)
    |> minByFirst
    |> Result.map \(_, digit) -> digit

findLast : Str -> Result Nat [ListWasEmpty]
findLast = \line ->
    digitNames
    |> List.keepOks (lastIndexOf line)
    |> minByFirst
    |> Result.map \(_, digit) -> digit

minByFirst : List (Num a, b) -> Result (Num a, b) [ListWasEmpty]
minByFirst = \list ->
    current, (index, digit) <- List.walk list (Err ListWasEmpty)

    when current is
        Ok (currentIndex, _) if index >= currentIndex ->
            current

        _ ->
            Ok (index, digit)

firstIndexOf : Str -> ((Str, Nat) -> Result (Nat, Nat) [NotFound])
firstIndexOf = \str ->
    \(name, digit) ->
        when Str.splitFirst str name is
            Ok { before } ->
                Ok (Str.countUtf8Bytes before, digit)

            _ ->
                Err NotFound

lastIndexOf : Str -> ((Str, Nat) -> Result (Nat, Nat) [NotFound])
lastIndexOf = \str ->
    \(name, digit) ->
        when Str.splitLast str name is
            Ok { after } ->
                Ok (Str.countUtf8Bytes after, digit)

            _ ->
                Err NotFound
