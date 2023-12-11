app "advent-of-code"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.6.2/c7T4Hp8bAdWz3r9ZrhboBzibCjJag8d0IP_ljb42yVc.tar.br",
        parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.3.0/-e3ebWWmlFPfe9fYrr2z1urfslzygbtQQsl69iH1qzQ.tar.br",
    }
    imports [
        pf.Stdout,
        pf.Task.{ Task },
        parser.Core.{ skip, many, oneOf, map },
        parser.String.{ Utf8, parseStr, codeunit },
        "../../inputs/day10.txt" as inputFile : Str,
    ]
    provides [main] to pf

Parser a : Core.Parser Utf8 a

Tile : [
    Vertical,
    Horizontal,
    NorthEast,
    NorthWest,
    SouthWest,
    SouthEast,
    Ground,
    Start,
]

Pos : (I32, I32)

Grid : List (List Tile)

Sets : {
    seen : Set Pos,
    left : Set Pos,
    right : Set Pos,
}

State : {
    seen : Set Pos,
    left : Set Pos,
    right : Set Pos,
    head : Pos,
}

main : Task {} *
main =
    when parseStr problemParser inputFile is
        Ok problem ->
            problem
            |> findSides
            |> findInside
            |> Set.len
            |> Num.toStr
            |> Stdout.line

        Err (ParsingIncomplete rest) ->
            Stdout.line "Parsing incomplete: \(rest)"

        Err (ParsingFailure _) ->
            Stdout.line "Parsing error"

problemParser : Parser Grid
problemParser =
    many rowParser

rowParser : Parser (List Tile)
rowParser =
    many tileParser
    |> skip (codeunit '\n')

tileParser : Parser Tile
tileParser =
    oneOf [
        codeunit '|' |> map \_ -> Vertical,
        codeunit '-' |> map \_ -> Horizontal,
        codeunit 'L' |> map \_ -> NorthEast,
        codeunit 'J' |> map \_ -> NorthWest,
        codeunit '7' |> map \_ -> SouthWest,
        codeunit 'F' |> map \_ -> SouthEast,
        codeunit '.' |> map \_ -> Ground,
        codeunit 'S' |> map \_ -> Start,
    ]

findInside : Sets -> Set Pos
findInside = \{ left, seen, right } ->
    newLeft = expandAll seen left
    newRight = expandAll seen right

    if newLeft == left then
        newLeft
    else if newRight == right then
        newRight
    else
        findInside {
            left: newLeft,
            seen,
            right: newRight,
        }

expandAll : Set Pos, Set Pos -> Set Pos
expandAll = \seen, set ->
    set
    |> Set.toList
    |> List.joinMap expand
    |> Set.fromList
    |> Set.difference seen

expand : Pos -> List Pos
expand = \(i, j) -> [
    (i, j),
    (i - 1, j),
    (i, j - 1),
    (i + 1, j),
    (i, j + 1),
]

findSides : Grid -> Sets
findSides = \grid ->
    head =
        getNext grid (getStart grid)
        |> List.first
        |> Result.withDefault (0, 0)

    findSidesHelper grid {
        seen: Set.single head,
        left: Set.empty {},
        right: Set.empty {},
        head,
    }

findSidesHelper : Grid, State -> Sets
findSidesHelper = \grid, state ->
    newHead =
        getNext grid state.head
        |> List.dropIf \pos -> Set.contains state.seen pos
        |> List.first

    when newHead is
        Ok head ->
            (newLeft, newRight) = getLeftRight grid state.head head

            findSidesHelper grid {
                seen: Set.insert state.seen head,
                left: Set.union state.left newLeft,
                right: Set.union state.right newRight,
                head,
            }

        Err _ ->
            {
                seen: state.seen,
                left: Set.difference state.left state.seen,
                right: Set.difference state.right state.seen,
            }

getStart : Grid -> Pos
getStart = \grid ->
    List.mapWithIndex grid \row, i ->
        List.mapWithIndex row \val, j ->
            when val is
                Start -> Ok (Num.toI32 i, Num.toI32 j)
                _ -> Err NotStart
        |> List.keepOks \x -> x
    |> List.join
    |> List.first
    |> Result.withDefault (0, 0)

getTile : Grid, Pos -> Tile
getTile = \grid, (i, j) ->
    if i < 0 || j < 0 then
        Ground
    else
        List.get grid (Num.toNat i)
        |> Result.try \row -> List.get row (Num.toNat j)
        |> Result.withDefault Ground

getLeftRight : Grid, Pos, Pos -> (Set Pos, Set Pos)
getLeftRight = \grid, (i, j), newPos ->
    north = (i - 1, j)
    east = (i, j + 1)
    south = (i + 1, j)
    west = (i, j - 1)

    when getTile grid (i, j) is
        Vertical if newPos == north -> (Set.single west, Set.single east)
        Vertical if newPos == south -> (Set.single east, Set.single west)
        Horizontal if newPos == west -> (Set.single south, Set.single north)
        Horizontal if newPos == east -> (Set.single north, Set.single south)
        NorthEast if newPos == north -> (Set.fromList [west, south], Set.empty {})
        NorthEast if newPos == east -> (Set.empty {}, Set.fromList [west, south])
        NorthWest if newPos == north -> (Set.empty {}, Set.fromList [east, south])
        NorthWest if newPos == west -> (Set.fromList [east, south], Set.empty {})
        SouthWest if newPos == south -> (Set.fromList [east, north], Set.empty {})
        SouthWest if newPos == west -> (Set.empty {}, Set.fromList [east, north])
        SouthEast if newPos == south -> (Set.empty {}, Set.fromList [west, north])
        SouthEast if newPos == east -> (Set.fromList [west, north], Set.empty {})
        _ -> (Set.empty {}, Set.empty {})

getNext : Grid, Pos -> List Pos
getNext = \grid, pos ->
    getPossibleNext grid pos
    |> List.keepIf \newPos ->
        getPossibleNext grid newPos
        |> List.contains pos

getPossibleNext : Grid, Pos -> List Pos
getPossibleNext = \grid, (i, j) ->
    when getTile grid (i, j) is
        Vertical -> [(i - 1, j), (i + 1, j)]
        Horizontal -> [(i, j - 1), (i, j + 1)]
        NorthEast -> [(i - 1, j), (i, j + 1)]
        NorthWest -> [(i - 1, j), (i, j - 1)]
        SouthWest -> [(i + 1, j), (i, j - 1)]
        SouthEast -> [(i + 1, j), (i, j + 1)]
        Ground -> []
        Start ->
            [
                (i - 1, j),
                (i, j - 1),
                (i, j + 1),
                (i + 1, j),
            ]
