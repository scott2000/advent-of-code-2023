app "advent-of-code"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.6.2/c7T4Hp8bAdWz3r9ZrhboBzibCjJag8d0IP_ljb42yVc.tar.br" }
    imports [
        pf.Stdout,
        pf.Task.{ Task },
        "../../inputs/day3.txt" as inputFile : Str,
    ]
    provides [main] to pf

Grid : List (List Str)

SymbolPositions : Dict (Nat, Nat) (Set (Nat, Nat))

GearRatios : Dict (Nat, Nat) Nat

ParseNumState : {
    currentNum : Nat,
    adjacentSymbols : Set (Nat, Nat),
    gearRatios : GearRatios,
}

main : Task {} *
main =
    inputFile
    |> parseGrid
    |> findNumsAdjacentToSymbol
    |> getGearRatios
    |> Dict.walk 0 (\state, _, v -> state + v)
    |> Num.toStr
    |> Stdout.line

parseGrid : Str -> Grid
parseGrid = \str ->
    str
    |> Str.split "\n"
    |> List.map Str.trim
    |> List.dropIf Str.isEmpty
    |> List.map Str.graphemes

findSymbolPositions : Grid -> SymbolPositions
findSymbolPositions = \grid ->
    outerState, row, i <- List.walkWithIndex grid (Dict.empty {})
    innerState, val, j <- List.walkWithIndex row outerState
    if !(isGearSymbol val) then
        innerState
    else if countAdjacentParts grid i j != 2 then
        innerState
    else
        insertAllAdjacent innerState i j

countAdjacentParts : Grid, Nat, Nat -> Nat
countAdjacentParts = \grid, i, j ->
    outerState, i1 <- List.walk [i - 1, i, i + 1] 0

    (_, result) = List.walk [j - 1, j, j + 1] (Bool.false, outerState) \(hasDigitBefore, innerState), j1 ->
        digit =
            row <- grid |> List.get i1 |> Result.try
            val <- row |> List.get j1 |> Result.try
            Str.toNat val

        when digit is
            Ok _ ->
                if hasDigitBefore then
                    (Bool.true, innerState)
                else
                    (Bool.true, innerState + 1)

            Err _ ->
                (Bool.false, innerState)

    result

insertAllAdjacent : SymbolPositions, Nat, Nat -> SymbolPositions
insertAllAdjacent = \state, i, j ->
    outerState, i1 <- List.walk [i - 1, i, i + 1] state
    innerState, j1 <- List.walk [j - 1, j, j + 1] outerState
    currentElem <- Dict.update innerState (i1, j1)
    when currentElem is
        Missing ->
            Set.single (i, j)
            |> Present

        Present set ->
            set
            |> Set.insert (i, j)
            |> Present

isGearSymbol : Str -> Bool
isGearSymbol = \val ->
    val == "*"

findNumsAdjacentToSymbol : Grid -> ParseNumState
findNumsAdjacentToSymbol = \grid ->
    symbolPositions = findSymbolPositions grid
    outerState, row, i <- List.walkWithIndex grid newState
    innerState, val, j <- List.walkWithIndex row (finalizeNumber outerState)
    addDigitToState innerState symbolPositions val i j

newState : ParseNumState
newState = {
    currentNum: 0,
    adjacentSymbols: Set.empty {},
    gearRatios: Dict.empty {},
}

addDigitToState : ParseNumState, SymbolPositions, Str, Nat, Nat -> ParseNumState
addDigitToState = \state, symbols, val, i, j ->
    when Str.toNat val is
        Ok digit ->
            adjacentSymbols =
                symbols
                |> Dict.get (i, j)
                |> Result.map \adj -> Set.union state.adjacentSymbols adj
                |> Result.withDefault state.adjacentSymbols

            { state &
                currentNum: state.currentNum * 10 + digit,
                adjacentSymbols,
            }

        Err _ ->
            finalizeNumber state

finalizeNumber : ParseNumState -> ParseNumState
finalizeNumber = \state ->
    { newState & gearRatios: getGearRatios state }

getGearRatios : ParseNumState -> GearRatios
getGearRatios = \state ->
    gearRatios, gearPosition <- Set.walk state.adjacentSymbols state.gearRatios
    currentRatio <- Dict.update gearRatios gearPosition
    when currentRatio is
        Missing ->
            Present state.currentNum

        Present curr ->
            Present (curr * state.currentNum)
