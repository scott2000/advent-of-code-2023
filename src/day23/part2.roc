app "advent-of-code"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.6.2/c7T4Hp8bAdWz3r9ZrhboBzibCjJag8d0IP_ljb42yVc.tar.br",
        parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.3.0/-e3ebWWmlFPfe9fYrr2z1urfslzygbtQQsl69iH1qzQ.tar.br",
    }
    imports [
        pf.Stdout,
        pf.Task.{ Task },
        parser.Core.{ const, skip, oneOrMore, oneOf },
        parser.String.{ Utf8, parseStr, codeunit },
        "../../inputs/day23.txt" as inputFile : Str,
    ]
    provides [main] to pf

Parser a : Core.Parser Utf8 a

Tile : [
    Path,
    Forest,
]

Grid : List (List Tile)

main : Task {} *
main =
    when parseStr problemParser inputFile is
        Ok problem ->
            problem
            |> buildGraph
            |> findLongestPath
            |> Num.toStr
            |> Stdout.line

        Err (ParsingIncomplete rest) ->
            Stdout.line "Parsing incomplete: \(rest)"

        Err (ParsingFailure error) ->
            Stdout.line "Parsing error: \(error)"

problemParser : Parser Grid
problemParser =
    oneOrMore rowParser

rowParser : Parser (List Tile)
rowParser =
    oneOrMore tileParser
    |> skip (codeunit '\n')

tileParser : Parser Tile
tileParser =
    oneOf [
        const Path |> skip (codeunit '.'),
        const Path |> skip (codeunit '^'),
        const Path |> skip (codeunit '>'),
        const Path |> skip (codeunit 'v'),
        const Path |> skip (codeunit '<'),
        const Forest |> skip (codeunit '#'),
    ]

Pos : (I32, I32)

getTile : Grid, Pos -> Result Tile [NotFound]
getTile = \grid, (i, j) ->
    List.get grid (Num.toNat i)
    |> Result.try \row -> List.get row (Num.toNat j)
    |> Result.mapErr \_ -> NotFound

Vertex : Nat

Graph : {
    vertices : List Pos,
    edges : List (List { dest : Vertex, cost : Nat }),
}

buildGraph : Grid -> Graph
buildGraph = \grid ->
    vertices = findVertices grid

    edges =
        List.map vertices \pos ->
            findEdges grid vertices pos (Set.single pos) 0

    { vertices, edges }

findVertices : Grid -> List Pos
findVertices = \grid ->
    List.mapWithIndex grid \row, i -> (row, i)
    |> List.joinMap \(row, i) ->
        List.mapWithIndex row \val, j -> (val, j)
        |> List.keepIf \(tile, _) -> tile == Path
        |> List.keepOks \(_, j) ->
            pos = (Num.toI32 i, Num.toI32 j)

            if List.len (getAdjacentPaths grid pos) == 2 then
                Err NotVertex
            else
                Ok pos

findEdges : Grid, List Pos, Pos, Set Pos, Nat -> List { dest : Vertex, cost : Nat }
findEdges = \grid, vertices, pos, seen, cost ->
    getAdjacentPaths grid pos
    |> List.dropIf \newPos -> Set.contains seen newPos
    |> List.joinMap \newPos ->
        when List.findFirstIndex vertices \vertex -> vertex == newPos is
            Ok dest ->
                [{ dest, cost: cost + 1 }]

            Err _ ->
                findEdges grid vertices newPos (Set.insert seen newPos) (cost + 1)

getAdjacentPaths : Grid, Pos -> List Pos
getAdjacentPaths = \grid, (i, j) ->
    north = (i - 1, j)
    south = (i + 1, j)
    west = (i, j - 1)
    east = (i, j + 1)

    [north, south, west, east]
    |> List.keepIf \pos -> getTile grid pos == Ok Path

findLongestPath : Graph -> Nat
findLongestPath = \graph ->
    findLongestPathHelper graph [0] 0 (List.len graph.vertices - 1) 0

findLongestPathHelper : Graph, List Nat, Vertex, Vertex, Nat -> Nat
findLongestPathHelper = \graph, seen, vertex, end, cost ->
    if vertex == end then
        cost
    else
        List.get graph.edges vertex
        |> Result.withDefault []
        |> List.dropIf \next -> List.contains seen next.dest
        |> List.map \next ->
            findLongestPathHelper graph (List.append seen next.dest) next.dest end (cost + next.cost)
        |> List.max
        |> Result.withDefault 0
