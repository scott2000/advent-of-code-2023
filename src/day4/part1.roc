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
    id : Nat,
    winningNumbers : Set Nat,
    selectedNumbers : Set Nat,
}

main : Task {} *
main =
    inputFile
    |> Str.split "\n"
    |> List.dropIf Str.isEmpty
    |> List.keepOks (\str -> parseStr cardParser str)
    |> List.map scoreCard
    |> List.sum
    |> Num.toStr
    |> Stdout.line

cardParser : Parser Card
cardParser =
    const (\id -> \winningNumbers -> \selectedNumbers -> { id, winningNumbers, selectedNumbers })
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

scoreCard : Card -> Nat
scoreCard = \card ->
    winningCount =
        card.selectedNumbers
        |> Set.intersection card.winningNumbers
        |> Set.len
        |> Num.toU8

    if winningCount == 0 then
        0
    else
        Num.shiftLeftBy 1 (winningCount - 1)
