app "advent-of-code"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.6.2/c7T4Hp8bAdWz3r9ZrhboBzibCjJag8d0IP_ljb42yVc.tar.br",
        parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.3.0/-e3ebWWmlFPfe9fYrr2z1urfslzygbtQQsl69iH1qzQ.tar.br",
    }
    imports [
        pf.Stdout,
        pf.Task.{ Task },
        parser.Core.{ const, skip, keep, map, oneOf, sepBy1, chompWhile },
        parser.String.{ Utf8, parseStr, strFromUtf8, codeunit, digit },
        "../../inputs/day15.txt" as inputFile : Str,
    ]
    provides [main] to pf

Parser a : Core.Parser Utf8 a

Operation : [
    Remove Str,
    Insert Str Nat,
]

LensBox : List (Str, Nat)

Table : List LensBox

main : Task {} *
main =
    when parseStr problemParser inputFile is
        Ok problem ->
            problem
            |> applyAll
            |> power
            |> Num.toStr
            |> Stdout.line

        Err (ParsingIncomplete rest) ->
            Stdout.line "Parsing incomplete: \(rest)"

        Err (ParsingFailure _) ->
            Stdout.line "Parsing error"

problemParser : Parser (List Operation)
problemParser =
    sepBy1 operationParser (codeunit ',')
    |> skip (codeunit '\n')

operationParser : Parser Operation
operationParser =
    oneOf [
        removeParser,
        insertParser,
    ]

removeParser : Parser Operation
removeParser =
    const Remove
    |> keep keyParser
    |> skip (codeunit '-')

insertParser : Parser Operation
insertParser =
    const (\key -> \val -> Insert key val)
    |> keep keyParser
    |> skip (codeunit '=')
    |> keep digit

keyParser : Parser Str
keyParser =
    chompWhile \ch -> ch >= 'a' && ch <= 'z'
    |> map strFromUtf8

hash : Str -> Nat
hash = \str ->
    hashHelper (Str.toUtf8 str) 0

hashHelper : List U8, Nat -> Nat
hashHelper = \str, acc ->
    when str is
        [] ->
            acc

        [head, .. as tail] ->
            hashHelper tail ((acc + Num.toNat head) * 17 % 256)

remove : LensBox, Str -> LensBox
remove = \box, str ->
    List.dropIf box \(key, _) -> key == str

insert : LensBox, Str, Nat -> LensBox
insert = \box, str, val ->
    when List.findFirstIndex box \(key, _) -> key == str is
        Ok index ->
            List.set box index (str, val)

        Err NotFound ->
            List.append box (str, val)

applyToBox : LensBox, Operation -> LensBox
applyToBox = \box, op ->
    when op is
        Remove key ->
            remove box key

        Insert key val ->
            insert box key val

applyToTable : Table, Operation -> Table
applyToTable = \table, op ->
    key =
        when op is
            Remove k -> k
            Insert k _ -> k

    hashed = hash key

    List.update table hashed \box -> applyToBox box op

applyAll : List Operation -> Table
applyAll = \ops ->
    List.walk ops (List.repeat [] 256) applyToTable

power : Table -> Nat
power = \table ->
    List.mapWithIndex table \box, boxIndex ->
        List.mapWithIndex box \(_, focalLength), lensIndex ->
            (boxIndex + 1) * (lensIndex + 1) * focalLength
        |> List.sum
    |> List.sum
