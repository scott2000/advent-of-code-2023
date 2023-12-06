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
    srcEnd : Nat,
}

SeedRange : {
    start : Nat,
    end : Nat,
}

Mapping : List MapRange

Problem : {
    seeds : List SeedRange,
    mappings : List Mapping,
}

main : Task {} *
main =
    when parseStr problemParser inputFile is
        Ok problem ->
            problem
            |> mapAllSeeds
            |> List.map .start
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

seedParser : Parser (List SeedRange)
seedParser =
    const (\seeds -> seeds)
    |> skip (string "seeds:")
    |> skip spaces
    |> keep (many seedRangeParser)
    |> skip (codeunit '\n')

seedRangeParser : Parser SeedRange
seedRangeParser =
    const (\start -> \len -> { start, end: start + len })
    |> keep digits
    |> skip spaces
    |> keep digits
    |> skip spaces

mappingParser : Parser Mapping
mappingParser =
    const (\mapping -> mapping)
    |> skip (chompUntil ':')
    |> skip (string ":\n")
    |> keep (many mapRangeParser)

mapRangeParser : Parser MapRange
mapRangeParser =
    const (\destStart -> \srcStart -> \len -> { destStart, srcStart, srcEnd: srcStart + len })
    |> keep digits
    |> skip spaces
    |> keep digits
    |> skip spaces
    |> keep digits
    |> skip spaces
    |> skip (codeunit '\n')

spaces : Parser {}
spaces = ignore (many (codeunit ' '))

mapAllSeeds : Problem -> List SeedRange
mapAllSeeds = \problem ->
    List.walk problem.mappings problem.seeds applyMapping

ApplyState : {
    unmapped : List SeedRange,
    mapped : List SeedRange,
}

emptyState : ApplyState
emptyState = {
    unmapped: [],
    mapped: [],
}

mergeState : ApplyState, ApplyState -> ApplyState
mergeState = \a, b -> {
    unmapped: List.concat a.unmapped b.unmapped,
    mapped: List.concat a.mapped b.mapped,
}

applyMapping : List SeedRange, Mapping -> List SeedRange
applyMapping = \seeds, mapping ->
    { unmapped, mapped } =
        List.walk mapping { emptyState & unmapped: seeds } applyMapRangeToEach

    List.concat unmapped mapped

applyMapRangeToEach : ApplyState, MapRange -> ApplyState
applyMapRangeToEach = \state, mapRange ->
    state.unmapped
    |> List.joinMap \s -> splitSeedRange s mapRange.srcStart
    |> List.joinMap \s -> splitSeedRange s mapRange.srcEnd
    |> List.map \s -> applyMapRange s mapRange
    |> List.walk { state & unmapped: [] } mergeState

applyMapRange : SeedRange, MapRange -> ApplyState
applyMapRange = \seed, mapRange ->
    if seed.start >= mapRange.srcStart && seed.end <= mapRange.srcEnd then
        { emptyState &
            mapped: [
                {
                    start: applyMapRangeSingle mapRange seed.start,
                    end: applyMapRangeSingle mapRange seed.end,
                },
            ],
        }
    else
        { emptyState & unmapped: [seed] }

splitSeedRange : SeedRange, Nat -> List SeedRange
splitSeedRange = \seed, at ->
    if at <= seed.start || at > seed.end then
        [seed]
    else
        [
            { seed & end: at },
            { seed & start: at },
        ]

applyMapRangeSingle : MapRange, Nat -> Nat
applyMapRangeSingle = \mapRange, num ->
    num - mapRange.srcStart + mapRange.destStart
