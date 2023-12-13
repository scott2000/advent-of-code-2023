app "advent-of-code"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.6.2/c7T4Hp8bAdWz3r9ZrhboBzibCjJag8d0IP_ljb42yVc.tar.br",
        parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.3.0/-e3ebWWmlFPfe9fYrr2z1urfslzygbtQQsl69iH1qzQ.tar.br",
    }
    imports [
        pf.Stdout,
        pf.Task.{ Task },
        parser.Core.{ const, skip, oneOrMore, oneOf, sepBy1 },
        parser.String.{ Utf8, parseStr, codeunit },
        "../../inputs/day13.txt" as inputFile : Str,
    ]
    provides [main] to pf

Parser a : Core.Parser Utf8 a

Tile : [
    Ash,
    Rock,
]

Grid : List (List Tile)

main : Task {} *
main =
    when parseStr problemParser inputFile is
        Ok problem ->
            problem
            |> List.map findReflectionLine
            |> List.sum
            |> Num.toStr
            |> Stdout.line

        Err (ParsingIncomplete rest) ->
            Stdout.line "Parsing incomplete: \(rest)"

        Err (ParsingFailure _) ->
            Stdout.line "Parsing error"

problemParser : Parser (List Grid)
problemParser =
    sepBy1 gridParser (codeunit '\n')

gridParser : Parser Grid
gridParser =
    oneOrMore rowParser

rowParser : Parser (List Tile)
rowParser =
    oneOrMore tileParser
    |> skip (codeunit '\n')

tileParser : Parser Tile
tileParser =
    oneOf [
        const Ash |> skip (codeunit '.'),
        const Rock |> skip (codeunit '#'),
    ]

findReflectionLine : Grid -> Nat
findReflectionLine = \grid ->
    vertScore =
        grid
        |> checkForReflection
        |> Result.withDefault 0

    horizScore =
        grid
        |> transpose
        |> checkForReflection
        |> Result.map \index -> 100 * index
        |> Result.withDefault 0

    vertScore + horizScore

getWidth : Grid -> Nat
getWidth = \grid ->
    grid
    |> List.get 0
    |> Result.map List.len
    |> Result.withDefault 0

transpose : Grid -> Grid
transpose = \grid ->
    List.range { start: At 0, end: Length (getWidth grid) }
    |> List.map \col ->
        List.mapTry grid \row ->
            List.get row col
        |> Result.withDefault []

checkForReflection : Grid -> Result Nat [NoReflection]
checkForReflection = \grid ->
    possibleIndices = List.range { start: At 1, end: Length (getWidth grid - 1) }

    List.walk grid possibleIndices findPossibleReflectionPoints
    |> List.first
    |> Result.mapErr \_ -> NoReflection

findPossibleReflectionPoints : List Nat, List Tile -> List Nat
findPossibleReflectionPoints = \currentPossible, tiles ->
    currentPossible
    |> List.keepIf \index -> isReflectionPoint tiles index

isReflectionPoint : List Tile, Nat -> Bool
isReflectionPoint = \tiles, index ->
    { before, others } = List.split tiles index
    size = Num.min (List.len before) (List.len others)
    List.takeLast before size == List.reverse (List.takeFirst others size)
