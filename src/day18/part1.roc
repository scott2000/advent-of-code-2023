app "advent-of-code"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.6.2/c7T4Hp8bAdWz3r9ZrhboBzibCjJag8d0IP_ljb42yVc.tar.br",
        parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.3.0/-e3ebWWmlFPfe9fYrr2z1urfslzygbtQQsl69iH1qzQ.tar.br",
    }
    imports [
        pf.Stdout,
        pf.Task.{ Task },
        parser.Core.{ const, keep, skip, oneOf, oneOrMore, chompWhile },
        parser.String.{ Utf8, parseStr, codeunit, string, digits, strFromUtf8 },
        "../../inputs/day18.txt" as inputFile : Str,
    ]
    provides [main] to pf

Parser a : Core.Parser Utf8 a

Direction : [
    North,
    South,
    West,
    East,
]

Operation : {
    dir : Direction,
    count : Nat,
    color : Str,
}

main : Task {} *
main =
    when parseStr problemParser inputFile is
        Ok problem ->
            problem
            |> findBorderSets
            |> findInterior
            |> Set.len
            |> Num.toStr
            |> Stdout.line

        Err (ParsingIncomplete rest) ->
            Stdout.line "Parsing incomplete: \(rest)"

        Err (ParsingFailure error) ->
            Stdout.line "Parsing error: \(error)"

problemParser : Parser (List Operation)
problemParser =
    oneOrMore operationParser

operationParser : Parser Operation
operationParser =
    const (\dir -> \count -> \color -> { dir, count, color })
    |> keep directionParser
    |> skip (codeunit ' ')
    |> keep digits
    |> skip (string " (")
    |> keep colorParser
    |> skip (string ")\n")

directionParser : Parser Direction
directionParser =
    oneOf [
        const North |> skip (codeunit 'U'),
        const South |> skip (codeunit 'D'),
        const West |> skip (codeunit 'L'),
        const East |> skip (codeunit 'R'),
    ]

colorParser : Parser Str
colorParser =
    const strFromUtf8
    |> skip (codeunit '#')
    |> keep (chompWhile \ch -> (ch >= '0' && ch <= '9') || (ch >= 'a' && ch <= 'f'))

Pos : (I32, I32)

BorderSets a : {
    border : Set Pos,
    left : Set Pos,
    right : Set Pos,
}a

State : BorderSets {
    pos : Pos,
}

findInterior : BorderSets * -> Set Pos
findInterior = \sets ->
    left = expand sets.left |> Set.difference sets.border
    right = expand sets.right |> Set.difference sets.border

    if left == sets.left then
        Set.union sets.border left
    else if right == sets.right then
        Set.union sets.border right
    else
        findInterior { sets & left, right }

expand : Set Pos -> Set Pos
expand = \set ->
    Set.joinMap set \(x, y) ->
        Set.fromList [
            (x - 1, y),
            (x + 1, y),
            (x, y - 1),
            (x, y + 1),
        ]

findBorderSets : List Operation -> BorderSets {}
findBorderSets = \ops ->
    initialState = {
        pos: (0, 0),
        border: Set.single (0, 0),
        left: Set.empty {},
        right: Set.empty {},
    }

    { border, left, right } = List.walk ops initialState applyOperation

    {
        border,
        left: Set.difference left border,
        right: Set.difference right border,
    }

applyOperation : State, Operation -> State
applyOperation = \state, op ->
    applyOperationHelper state op 0

applyOperationHelper : State, Operation, Nat -> State
applyOperationHelper = \state, op, n ->
    if n == op.count then
        state
    else
        { next, left, right } = getAdjacent state.pos op.dir

        applyOperationHelper
            {
                pos: next,
                border: Set.insert state.border next,
                left: Set.insert state.left left,
                right: Set.insert state.right right,
            }
            op
            (n + 1)

getAdjacent : Pos, Direction -> { next : Pos, left : Pos, right : Pos }
getAdjacent = \(x, y), dir ->
    north = (x, y - 1)
    south = (x, y + 1)
    west = (x - 1, y)
    east = (x + 1, y)

    when dir is
        North -> { next: north, left: west, right: east }
        South -> { next: south, left: east, right: west }
        West -> { next: west, left: south, right: north }
        East -> { next: east, left: north, right: south }
