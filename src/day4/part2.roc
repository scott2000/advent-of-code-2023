app "advent-of-code"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.6.2/c7T4Hp8bAdWz3r9ZrhboBzibCjJag8d0IP_ljb42yVc.tar.br",
        parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.2.0/dJQSsSmorujhiPNIvJKlQoI92RFIG_JQwUfIxZsCSwE.tar.br",
    }
    imports [
        pf.Stdout,
        pf.Task.{ Task },
        parser.Core.{ const, keep, skip, ignore, map, many },
        parser.String.{ RawStr, parseStr, codeunit, string, digits },
        "../../inputs/day4.txt" as inputFile : Str,
    ]
    provides [main] to pf

Parser a : Core.Parser RawStr a

Card : {
    index : Nat,
    winningNumbers : Set Nat,
    selectedNumbers : Set Nat,
}

CardCounts : List Nat

main : Task {} *
main =
    cards =
        inputFile
        |> Str.split "\n"
        |> List.dropIf Str.isEmpty
        |> List.keepOks (\str -> parseStr cardParser str)

    cards
    |> List.walk (initialCardCounts cards) scoreCard
    |> List.sum
    |> Num.toStr
    |> Stdout.line

cardParser : Parser Card
cardParser =
    const (\id -> \winningNumbers -> \selectedNumbers -> { index: id - 1, winningNumbers, selectedNumbers })
    |> skip (string "Card")
    |> skip spaces
    |> keep digits
    |> skip spaces
    |> skip (codeunit ':')
    |> skip spaces
    |> keep digitSet
    |> skip (codeunit '|')
    |> skip spaces
    |> keep digitSet

digitSet : Parser (Set Nat)
digitSet =
    many (digits |> skip spaces)
    |> map Set.fromList

spaces : Parser {}
spaces = ignore (many (codeunit ' '))

initialCardCounts : List Card -> CardCounts
initialCardCounts = \cards ->
    cards
    |> List.map \_ -> 1

scoreCard : CardCounts, Card -> CardCounts
scoreCard = \counts, card ->
    multiplier =
        List.get counts card.index
        |> Result.withDefault 0

    winCount =
        card.selectedNumbers
        |> Set.intersection card.winningNumbers
        |> Set.len

    copyIndices =
        List.range {
            start: At (card.index + 1),
            end: Length winCount,
        }

    counts1, index <- List.walk copyIndices counts
    List.update counts1 index \count -> count + multiplier
