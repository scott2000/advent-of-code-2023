app "advent-of-code"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.6.2/c7T4Hp8bAdWz3r9ZrhboBzibCjJag8d0IP_ljb42yVc.tar.br" }
    imports [
        pf.Stdout,
        pf.Task.{ Task },
        "../../inputs/day3.txt" as inputFile : Str,
    ]
    provides [main] to pf

Grid : List (List Str)

SymbolPositions : Set (Nat, Nat)

ParseNumState : {
    currentNum : Nat,
    isAdjacentToSymbol : Bool,
    parsedNums : List Nat,
}

main : Task {} *
main =
    inputFile
    |> parseGrid
    |> findNumsAdjacentToSymbol
    |> getParsedNums
    |> List.sum
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
    outerState, row, i <- List.walkWithIndex grid (Set.empty {})
    innerState, val, j <- List.walkWithIndex row outerState
    if isSymbol val then
        insertAllAdjacent innerState i j
    else
        innerState

insertAllAdjacent : SymbolPositions, Nat, Nat -> SymbolPositions
insertAllAdjacent = \state, i, j ->
    outerState, i1 <- List.walk [i - 1, i, i + 1] state
    innerState, j1 <- List.walk [j - 1, j, j + 1] outerState
    Set.insert innerState (i1, j1)

isSymbol : Str -> Bool
isSymbol = \val ->
    when val is
        "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" | "." -> Bool.false
        _ -> Bool.true

findNumsAdjacentToSymbol : Grid -> ParseNumState
findNumsAdjacentToSymbol = \grid ->
    symbolPositions = findSymbolPositions grid
    outerState, row, i <- List.walkWithIndex grid newState
    innerState, val, j <- List.walkWithIndex row (finalizeNumber outerState)
    addDigitToState innerState symbolPositions val i j

newState : ParseNumState
newState = {
    currentNum: 0,
    isAdjacentToSymbol: Bool.false,
    parsedNums: [],
}

addDigitToState : ParseNumState, SymbolPositions, Str, Nat, Nat -> ParseNumState
addDigitToState = \state, symbols, val, i, j ->
    when Str.toNat val is
        Ok digit ->
            isAdjacentToSymbol =
                if state.isAdjacentToSymbol then
                    Bool.true
                else
                    Set.contains symbols (i, j)

            { state &
                currentNum: state.currentNum * 10 + digit,
                isAdjacentToSymbol,
            }

        Err _ ->
            finalizeNumber state

getParsedNum : ParseNumState -> Result Nat [NoNewNumber]
getParsedNum = \state ->
    if state.currentNum > 0 && state.isAdjacentToSymbol then
        Ok state.currentNum
    else
        Err NoNewNumber

finalizeNumber : ParseNumState -> ParseNumState
finalizeNumber = \state ->
    { newState & parsedNums: getParsedNums state }

getParsedNums : ParseNumState -> List Nat
getParsedNums = \state ->
    state.parsedNums
    |> List.appendIfOk (getParsedNum state)