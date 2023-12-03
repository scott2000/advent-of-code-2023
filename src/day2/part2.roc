app "advent-of-code"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.6.2/c7T4Hp8bAdWz3r9ZrhboBzibCjJag8d0IP_ljb42yVc.tar.br" }
    imports [
        pf.Stdout,
        pf.Task.{ Task },
        "../../inputs/day2.txt" as inputFile : Str,
    ]
    provides [main] to pf

main : Task {} *
main =
    inputFile
    |> validGameIds
    |> List.sum
    |> Num.toStr
    |> Stdout.line

Color : [Red, Green, Blue]

Game : {
    id : Nat,
    rounds : List Round,
}

Round : List ColorCount

ColorCount : (Nat, Color)

Summary : {
    red : Nat,
    green : Nat,
    blue : Nat,
}

validGameIds : Str -> List Nat
validGameIds = \input ->
    input
    |> Str.split "\n"
    |> List.dropIf Str.isEmpty
    |> List.keepOks parseGame
    |> List.map summarizeGame
    |> List.map power

power : Summary -> Nat
power = \summary ->
    summary.red * summary.green * summary.blue

parseGame : Str -> Result Game [InvalidColor, InvalidNumStr, NotTwoParts]
parseGame = \str ->
    when Str.split str ":" is
        [gameInfo, rounds] ->
            parsedId <- parseGameId gameInfo |> Result.try
            parsedRounds <- parseRounds rounds |> Result.try
            Ok {
                id: parsedId,
                rounds: parsedRounds,
            }

        _ ->
            Err NotTwoParts

parseGameId : Str -> Result Nat [InvalidNumStr, NotTwoParts]
parseGameId = \str ->
    words =
        str
        |> Str.split " "
        |> List.dropIf Str.isEmpty

    when words is
        [_, id] -> Str.toNat id
        _ -> Err NotTwoParts

parseRounds : Str -> Result (List Round) [InvalidColor, InvalidNumStr, NotTwoParts]
parseRounds = \str ->
    str
    |> Str.split ";"
    |> List.mapTry parseRound

parseRound : Str -> Result Round [InvalidColor, InvalidNumStr, NotTwoParts]
parseRound = \str ->
    str
    |> Str.split ","
    |> List.mapTry parseColorCount

parseColorCount : Str -> Result ColorCount [InvalidColor, InvalidNumStr, NotTwoParts]
parseColorCount = \str ->
    words =
        str
        |> Str.split " "
        |> List.dropIf Str.isEmpty

    when words is
        [count, color] ->
            parsedCount <- Str.toNat count |> Result.try
            parsedColor <- parseColor color |> Result.try
            Ok (parsedCount, parsedColor)

        _ -> Err NotTwoParts

parseColor : Str -> Result Color [InvalidColor]
parseColor = \word ->
    when word is
        "red" -> Ok Red
        "green" -> Ok Green
        "blue" -> Ok Blue
        _ -> Err InvalidColor

summarizeGame : Game -> Summary
summarizeGame = \game ->
    game.rounds
    |> List.map summarizeRound
    |> List.walk emptySummary maxSummary

summarizeRound : Round -> Summary
summarizeRound = \round ->
    List.walk round emptySummary addColorCountToSummary

emptySummary : Summary
emptySummary = {
    red: 0,
    green: 0,
    blue: 0,
}

addColorCountToSummary : Summary, ColorCount -> Summary
addColorCountToSummary = \round, (count, color) ->
    when color is
        Red -> { round & red: round.red + count }
        Green -> { round & green: round.green + count }
        Blue -> { round & blue: round.blue + count }

maxSummary : Summary, Summary -> Summary
maxSummary = \a, b -> {
    red: Num.max a.red b.red,
    green: Num.max a.green b.green,
    blue: Num.max a.blue b.blue,
}
