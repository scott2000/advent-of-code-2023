app "advent-of-code"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.6.2/c7T4Hp8bAdWz3r9ZrhboBzibCjJag8d0IP_ljb42yVc.tar.br",
        parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.3.0/-e3ebWWmlFPfe9fYrr2z1urfslzygbtQQsl69iH1qzQ.tar.br",
    }
    imports [
        pf.Stdout,
        pf.Task.{ Task },
        parser.Core.{ const, skip, keep, map, oneOrMore, chompWhile },
        parser.String.{ Utf8, strFromUtf8, parseStr, codeunit },
        "../../inputs/day25.txt" as inputFile : Str,
    ]
    provides [main] to pf

Parser a : Core.Parser Utf8 a

Connection : {
    start : Str,
    end : List Str,
}

main : Task {} *
main =
    when parseStr problemParser inputFile is
        Ok problem ->
            problem
            |> solveProblem
            |> Num.toStr
            |> Stdout.line

        Err (ParsingIncomplete rest) ->
            Stdout.line "Parsing incomplete: \(rest)"

        Err (ParsingFailure error) ->
            Stdout.line "Parsing error: \(error)"

problemParser : Parser (List Connection)
problemParser =
    oneOrMore connectionParser

connectionParser : Parser Connection
connectionParser =
    const \start -> \end -> { start, end }
    |> keep nameParser
    |> skip (codeunit ':')
    |> keep (oneOrMore nameParserAfterSpace)
    |> skip (codeunit '\n')

nameParserAfterSpace : Parser Str
nameParserAfterSpace =
    const \name -> name
    |> skip (codeunit ' ')
    |> keep nameParser

nameParser : Parser Str
nameParser =
    chompWhile \ch -> ch >= 'a' && ch <= 'z'
    |> map strFromUtf8

Graph : Dict Str (List Str)

solveProblem : List Connection -> Nat
solveProblem = \connections ->
    firstVertex =
        when connections is
            [{ start }, ..] -> start
            _ ->
                crash "No connections"

    graph = buildGraph connections

    connectedSet =
        expandConnectedSet graph (Set.single firstVertex) (Set.empty {})

    Set.len connectedSet * (Dict.len graph - Set.len connectedSet)

expandConnectedSet : Graph, Set Str, Set Str -> Set Str
expandConnectedSet = \graph, connected, disconnected ->
    nextVertex =
        Set.walkUntil connected NoVertex \_, vertex ->
            result = findNextVertexFrom graph connected disconnected vertex

            when result is
                NoVertex -> Continue result
                _ -> Break result

    when nextVertex is
        NoVertex -> connected
        DisconnectedVertex v ->
            expandConnectedSet graph connected (Set.insert disconnected v)

        ConnectedVertex v ->
            expandConnectedSet graph (Set.insert connected v) disconnected

findNextVertexFrom : Graph, Set Str, Set Str, Str -> [NoVertex, ConnectedVertex Str, DisconnectedVertex Str]
findNextVertexFrom = \graph, connected, disconnected, vertex ->
    getAdjacent graph vertex
    |> List.findFirst \v -> !(Set.contains connected v) && !(Set.contains disconnected v)
    |> Result.map \next ->
        if isConnectedVertex graph connected next then
            ConnectedVertex next
        else
            DisconnectedVertex next
    |> Result.withDefault NoVertex

isConnectedVertex : Graph, Set Str, Str -> Bool
isConnectedVertex = \graph, connected, vertex ->
    isConnectedVertexHelper graph connected (Set.empty {}) 4 vertex
    |> Result.isOk

isConnectedVertexHelper : Graph, Set Str, Set (Str, Str), Nat, Str -> Result {} [NoPath]
isConnectedVertexHelper = \graph, connected, used, pathsRequired, vertex ->
    if pathsRequired == 0 then
        Ok {}
    else
        newUsed <- findPath graph connected used vertex |> Result.try

        isConnectedVertexHelper graph connected newUsed (pathsRequired - 1) vertex

findPath : Graph, Set Str, Set (Str, Str), Str -> Result (Set (Str, Str)) [NoPath]
findPath = \graph, connected, used, vertex ->
    findPathHelper graph connected (Set.single vertex) (Dict.single vertex used)

findPathHelper : Graph, Set Str, Set Str, Dict Str (Set (Str, Str)) -> Result (Set (Str, Str)) [NoPath]
findPathHelper = \graph, connected, seen, states ->
    if Dict.isEmpty states then
        Err NoPath
    else
        statesList = Dict.toList states

        when List.findFirst statesList \(vertex, _) -> Set.contains connected vertex is
            Ok (_, used) -> Ok used
            Err _ ->
                newStates =
                    statesList
                    |> List.joinMap \(vertex, used) -> findPathStep graph seen vertex used
                    |> Dict.fromList

                newSeen =
                    Dict.walk newStates seen \currentSeen, vertex, _ -> Set.insert currentSeen vertex

                findPathHelper graph connected newSeen newStates

findPathStep : Graph, Set Str, Str, Set (Str, Str) -> List (Str, Set (Str, Str))
findPathStep = \graph, seen, vertex, used ->
    getAdjacent graph vertex
    |> List.dropIf \v -> Set.contains used (vertex, v) || Set.contains used (v, vertex) || Set.contains seen v
    |> List.map \next -> (next, Set.insert used (vertex, next))

getAdjacent : Graph, Str -> List Str
getAdjacent = \graph, vertex ->
    Dict.get graph vertex
    |> Result.withDefault []

buildGraph : List Connection -> Graph
buildGraph = \connections ->
    List.walk connections (Dict.empty {}) addConnection

addConnection : Graph, Connection -> Graph
addConnection = \graph, connection ->
    List.walk connection.end graph \currentGraph, end ->
        addEdge currentGraph end [connection.start]
    |> addEdge connection.start connection.end

addEdge : Graph, Str, List Str -> Graph
addEdge = \graph, start, end ->
    endWithoutStart =
        List.dropIf end \e -> e == start

    Dict.update graph start \entry ->
        when entry is
            Present list -> Present (List.concat list endWithoutStart)
            Missing -> Present endWithoutStart
