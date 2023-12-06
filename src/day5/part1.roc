app "advent-of-code"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.6.2/c7T4Hp8bAdWz3r9ZrhboBzibCjJag8d0IP_ljb42yVc.tar.br",
        parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.3.0/-e3ebWWmlFPfe9fYrr2z1urfslzygbtQQsl69iH1qzQ.tar.br",
    }
    imports [
        pf.Stdout,
        pf.Task.{ Task },
        parser.Core.{ const, keep, skip, ignore, many, sepBy, chompUntil },
        parser.String.{ Utf8, parseStr, codeunit, string, digits },
        "../../inputs/day5.txt" as inputFile : Str,
    ]
    provides [main] to pf

Parser a : Core.Parser Utf8 a

MapRange : {
    destStart : Nat,
    srcStart : Nat,
    len : Nat,
}

Mapping : List MapRange

Problem : {
    seeds : List Nat,
    mappings : List Mapping,
}

main : Task {} *
main =
    when parseStr problemParser inputFile is
        Ok problem ->
            problem
            |> mapAllSeeds
            |> List.min
            |> Result.map Num.toStr
            |> Result.withDefault "No minimum found"
            |> Stdout.line

        Err _ ->
            Stdout.line "Parsing error"

problemParser : Parser Problem
problemParser =
    const (\seeds -> \mappings -> { seeds, mappings })
    |> keep seedParser
    |> skip (codeunit '\n')
    |> keep (sepBy mappingParser (codeunit '\n'))

seedParser : Parser (List Nat)
seedParser =
    const (\seeds -> seeds)
    |> skip (string "seeds:")
    |> skip spaces
    |> keep (sepBy digits (codeunit ' '))
    |> skip (codeunit '\n')

mappingParser : Parser Mapping
mappingParser =
    const (\mapping -> mapping)
    |> skip (chompUntil ':')
    |> skip (string ":\n")
    |> keep (many mapRangeParser)

mapRangeParser : Parser MapRange
mapRangeParser =
    const (\destStart -> \srcStart -> \len -> { destStart, srcStart, len })
    |> keep digits
    |> skip spaces
    |> keep digits
    |> skip spaces
    |> keep digits
    |> skip spaces
    |> skip (codeunit '\n')

spaces : Parser {}
spaces = ignore (many (codeunit ' '))

mapAllSeeds : Problem -> List Nat
mapAllSeeds = \problem ->
    problem.seeds
    |> List.map \seed -> applyMappings seed problem.mappings

applyMappings : Nat, List Mapping -> Nat
applyMappings = \num, mappings ->
    List.walk mappings num applyMapping

applyMapping : Nat, Mapping -> Nat
applyMapping = \num, mapping ->
    List.walkUntil mapping num applyMapRange

applyMapRange : Nat, MapRange -> [Break Nat, Continue Nat]
applyMapRange = \num, mapRange ->
    if num < mapRange.srcStart then
        Continue num
    else
        offset = num - mapRange.srcStart

        if offset >= mapRange.len then
            Continue num
        else
            Break (mapRange.destStart + offset)
