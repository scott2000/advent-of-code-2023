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
    |> List.map Str.toUtf8
    |> List.map parseDigits
    |> List.map parseNum
    |> List.sum

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

digitNamesUtf8 : List (List U8, Nat)
digitNamesUtf8 =
    digitNames
    |> List.map (\(name, digit) -> (Str.toUtf8 name, digit))

parseDigits : List U8 -> List Nat
parseDigits = \line ->
    if List.isEmpty line then
        []
    else
        when parseDigit line is
            (Ok digit, rest) -> parseDigits rest |> List.prepend digit
            (Err NoDigit, rest) -> parseDigits rest

parseDigit : List U8 -> (Result Nat [NoDigit], List U8)
parseDigit = \line ->
    rest = List.dropFirst line 1
    result, (prefix, digit) <- List.walkUntil digitNamesUtf8 (Err NoDigit, rest)

    if List.startsWith line prefix then
        Break (Ok digit, rest)
    else
        Continue result

parseNum : List Nat -> Nat
parseNum = \digits ->
    when digits is
        [a, .., b] -> a * 10 + b
        [a] -> a * 10 + a
        _ -> crash "Invalid num"
