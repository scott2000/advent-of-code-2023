app "advent-of-code"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.6.2/c7T4Hp8bAdWz3r9ZrhboBzibCjJag8d0IP_ljb42yVc.tar.br",
        parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.2.0/dJQSsSmorujhiPNIvJKlQoI92RFIG_JQwUfIxZsCSwE.tar.br",
    }
    imports [
        pf.Stdout,
        pf.Task.{ Task },
        parser.Core.{ sepBy, const, keep, skip, ignore, map, many },
        parser.String.{ RawStr, parseStr, codeunit, string, digits, oneOf },
        "../../inputs/day2.txt" as inputFile : Str,
    ]
    provides [main] to pf

Parser a : Core.Parser RawStr a

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
    |> List.keepOks (\str -> parseStr gameParser str)
    |> List.map summarizeGame
    |> List.map power

power : Summary -> Nat
power = \summary ->
    summary.red * summary.green * summary.blue

gameParser : Parser Game
gameParser =
    const (\id -> \rounds -> { id, rounds })
    |> keep gameIdParser
    |> skip (codeunit ':')
    |> keep (sepBy roundParser (codeunit ';'))

gameIdParser : Parser Nat
gameIdParser =
    const (\id -> id)
    |> skip (string "Game")
    |> skip spaces
    |> keep digits
    |> skip spaces

roundParser : Parser Round
roundParser =
    colorCountParser
    |> sepBy (codeunit ',')

colorCountParser : Parser ColorCount
colorCountParser =
    const (\count -> \color -> (count, color))
    |> skip spaces
    |> keep digits
    |> skip spaces
    |> keep colorParser
    |> skip spaces

colorParser : Parser Color
colorParser =
    oneOf [
        string "red" |> map \_ -> Red,
        string "green" |> map \_ -> Green,
        string "blue" |> map \_ -> Blue,
    ]

spaces : Parser {}
spaces = ignore (many (codeunit ' '))

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
