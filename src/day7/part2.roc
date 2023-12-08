app "advent-of-code"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.6.2/c7T4Hp8bAdWz3r9ZrhboBzibCjJag8d0IP_ljb42yVc.tar.br",
        parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.3.0/-e3ebWWmlFPfe9fYrr2z1urfslzygbtQQsl69iH1qzQ.tar.br",
    }
    imports [
        pf.Stdout,
        pf.Task.{ Task },
        parser.Core.{ const, keep, skip, ignore, many, flatten, oneOf, map },
        parser.String.{ Utf8, parseStr, codeunit, digit, digits },
        "../../inputs/day7.txt" as inputFile : Str,
    ]
    provides [main] to pf

Parser a : Core.Parser Utf8 a

Card : U8

Hand : (Card, Card, Card, Card, Card)

HandKind : [
    FiveOfAKind,
    FourOfAKind,
    FullHouse,
    ThreeOfAKind,
    TwoPair,
    OnePair,
    HighCard,
]

main : Task {} *
main =
    when parseStr problemParser inputFile is
        Ok problem ->
            problem
            |> List.sortWith sortBid
            |> List.map \(_, bid) -> bid
            |> List.mapWithIndex \bid, index -> bid * (index + 1)
            |> List.sum
            |> Num.toStr
            |> Stdout.line

        Err _ ->
            Stdout.line "Parsing error"

problemParser : Parser (List (Hand, Nat))
problemParser = many bidParser

bidParser : Parser (Hand, Nat)
bidParser =
    const (\hand -> \bid -> (hand, bid))
    |> keep handParser
    |> skip spaces
    |> keep digits
    |> skip (codeunit '\n')

handParser : Parser Hand
handParser =
    many cardParser
    |> map \cards ->
        when cards is
            [a, b, c, d, e] ->
                Ok (a, b, c, d, e)

            _ ->
                Err "Must be exactly five cards in a hand"
    |> flatten

cardParser : Parser Card
cardParser =
    oneOf [
        codeunit 'J' |> map \_ -> 0,
        digit |> map \d -> Num.toU8 d - 1,
        codeunit 'T' |> map \_ -> 9,
        codeunit 'Q' |> map \_ -> 10,
        codeunit 'K' |> map \_ -> 11,
        codeunit 'A' |> map \_ -> 12,
    ]

spaces : Parser {}
spaces = ignore (many (codeunit ' '))

sortBid : (Hand, Nat), (Hand, Nat) -> [LT, EQ, GT]
sortBid = \(a, _), (b, _) ->
    Num.compare (getHandStrength a) (getHandStrength b)

getHandStrength : Hand -> Nat
getHandStrength = \hand ->
    (a, b, c, d, e) = hand

    getHandKindMultiplier (getHandKind hand)
    |> addCard a
    |> addCard b
    |> addCard c
    |> addCard d
    |> addCard e

addCard : Nat, Card -> Nat
addCard = \strength, card ->
    strength * 13 + Num.toNat card

getHandKindMultiplier : HandKind -> Nat
getHandKindMultiplier = \kind ->
    when kind is
        FiveOfAKind -> 6
        FourOfAKind -> 5
        FullHouse -> 4
        ThreeOfAKind -> 3
        TwoPair -> 2
        OnePair -> 1
        HighCard -> 0

getHandKind : Hand -> HandKind
getHandKind = \hand ->
    when getCountIncludingJokers hand is
        [5] -> FiveOfAKind
        [4, 1] -> FourOfAKind
        [3, 2] -> FullHouse
        [3, 1, 1] -> ThreeOfAKind
        [2, 2, 1] -> TwoPair
        [2, 1, 1, 1] -> OnePair
        [1, 1, 1, 1, 1] -> HighCard
        _ ->
            crash "Invalid counts"

getCountIncludingJokers : Hand -> List Nat
getCountIncludingJokers = \hand ->
    getCounts hand
    |> List.append 0
    |> List.update 0 \n -> n + countJokers hand
    |> List.dropIf \n -> n == 0

countJokers : Hand -> Nat
countJokers = \(a, b, c, d, e) ->
    [a, b, c, d, e]
    |> List.countIf \card -> card == 0

getCounts : Hand -> List Nat
getCounts = \(a, b, c, d, e) ->
    Dict.empty {}
    |> countOccurances a
    |> countOccurances b
    |> countOccurances c
    |> countOccurances d
    |> countOccurances e
    |> Dict.values
    |> List.sortDesc

countOccurances : Dict Card Nat, Card -> Dict Card Nat
countOccurances = \dict, card ->
    if card == 0 then
        dict
    else
        Dict.update dict card \existing ->
            when existing is
                Missing ->
                    Present 1

                Present n ->
                    Present (n + 1)
