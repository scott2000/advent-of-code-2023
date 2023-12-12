app "advent-of-code"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.6.2/c7T4Hp8bAdWz3r9ZrhboBzibCjJag8d0IP_ljb42yVc.tar.br",
    }
    imports [
        pf.Stdout,
        pf.Task.{ Task },
        "../../inputs/day11.txt" as inputFile : Str,
    ]
    provides [main] to pf

Pos : (Nat, Nat)

main : Task {} *
main =
    inputFile
    |> parseInput
    |> expandSpace
    |> sumDistances
    |> Num.toStr
    |> Stdout.line

parseInput : Str -> List Pos
parseInput = \input ->
    input
    |> Str.split "\n"
    |> List.dropIf Str.isEmpty
    |> List.walkWithIndex [] \list, row, i ->
        Str.walkUtf8WithIndex row list \list1, val, j ->
            if val == '#' then
                List.append list1 (i, j)
            else
                list1

expandSpace : List Pos -> List Pos
expandSpace = \list ->
    iCoordDict = buildCoordDict list .0
    jCoordDict = buildCoordDict list .1

    List.map list \(i, j) ->
        (
            Dict.get iCoordDict i |> Result.withDefault i,
            Dict.get jCoordDict j |> Result.withDefault j,
        )

buildCoordDict : List Pos, (Pos -> Nat) -> Dict Nat Nat
buildCoordDict = \list, getCoord ->
    byCoord =
        list
        |> List.map getCoord
        |> Set.fromList

    List.range { start: At 0, end: At (Set.walk byCoord 0 Num.max) }
    |> List.walk { dict: Dict.empty {}, shift: 0 } \state, c ->
        if Set.contains byCoord c then
            { state & dict: Dict.insert state.dict c (c + state.shift) }
        else
            { state & shift: state.shift + 1 }
    |> .dict

sumDistances : List Pos -> Nat
sumDistances = \list ->
    List.walkWithIndex list 0 \acc, a, i ->
        List.walkFrom list (i + 1) acc \acc1, b ->
            acc1 + dist a b

dist : Pos, Pos -> Nat
dist = \(i1, j1), (i2, j2) ->
    Num.absDiff i1 i2 + Num.absDiff j1 j2
