app "advent-of-code"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.6.2/c7T4Hp8bAdWz3r9ZrhboBzibCjJag8d0IP_ljb42yVc.tar.br",
        parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.3.0/-e3ebWWmlFPfe9fYrr2z1urfslzygbtQQsl69iH1qzQ.tar.br",
    }
    imports [
        pf.Stdout,
        pf.Task.{ Task },
        parser.Core.{ const, skip, keep, oneOrMore },
        parser.String.{ Utf8, parseStr, codeunit, digits },
        "../../inputs/day22.txt" as inputFile : Str,
    ]
    provides [main] to pf

Parser a : Core.Parser Utf8 a

Pos : {
    x : Nat,
    y : Nat,
    z : Nat,
}

Brick : {
    start : Pos,
    end : Pos,
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

problemParser : Parser (List Brick)
problemParser =
    oneOrMore brickParser

brickParser : Parser Brick
brickParser =
    const \start -> \end -> { start, end }
    |> keep posParser
    |> skip (codeunit '~')
    |> keep posParser
    |> skip (codeunit '\n')

posParser : Parser Pos
posParser =
    const \x -> \y -> \z -> { x, y, z }
    |> keep digits
    |> skip (codeunit ',')
    |> keep digits
    |> skip (codeunit ',')
    |> keep digits

solveProblem : List Brick -> Nat
solveProblem = \bricks ->
    bricks
    |> sortBricks
    |> buildTower
    |> findRemovableBricks
    |> List.len

sortBricks : List Brick -> List Brick
sortBricks = \bricks ->
    List.sortWith bricks \a, b ->
        az = Num.min a.start.z a.end.z
        bz = Num.min b.start.z b.end.z
        Num.compare az bz

Tower : {
    bricks : Dict Nat Brick,
    below : Dict Nat (List Nat),
    above : Dict Nat (List Nat),
}

findRemovableBricks : Tower -> List Nat
findRemovableBricks = \tower ->
    Dict.walk tower.bricks [] \current, brick, _ ->
        canBeRemoved =
            Dict.get tower.above brick
            |> Result.withDefault []
            |> List.all \parent ->
                Dict.get tower.below parent
                |> Result.withDefault []
                |> List.any \child -> child != brick

        if canBeRemoved then
            List.append current brick
        else
            current

BuildTowerState : {
    tower : Tower,
    tallest : Dict (Nat, Nat) { brick : Nat, height : Nat },
}

buildTower : List Brick -> Tower
buildTower = \bricks ->
    initialState = {
        tower: {
            bricks: Dict.empty {},
            below: Dict.empty {},
            above: Dict.empty {},
        },
        tallest: Dict.empty {},
    }

    List.walk bricks initialState addBrick
    |> .tower

addBrick : BuildTowerState, Brick -> BuildTowerState
addBrick = \state, brick ->
    coords = getBaseCoords brick

    { bricks: below, height } = getTallestBelow state coords

    adjustedBrick = adjustHeight brick height

    (tower, index) = addBrickToTower state.tower adjustedBrick below

    tallest =
        List.walk coords state.tallest \current, coord ->
            Dict.insert current coord {
                brick: index,
                height: Num.max adjustedBrick.start.z adjustedBrick.end.z,
            }

    { tower, tallest }

adjustHeight : Brick, Nat -> Brick
adjustHeight = \brick, base ->
    startZ = base + 1
    endZ = startZ + Num.absDiff brick.start.z brick.end.z

    {
        start: setZ brick.start startZ,
        end: setZ brick.end endZ,
    }

setZ : Pos, Nat -> Pos
setZ = \pos, z -> { pos & z }

getBaseCoords : Brick -> List (Nat, Nat)
getBaseCoords = \brick ->
    List.range { start: At brick.start.x, end: At brick.end.x }
    |> List.joinMap \x ->
        List.range { start: At brick.start.y, end: At brick.end.y }
        |> List.map \y ->
            (x, y)

getTallestBelow : BuildTowerState, List (Nat, Nat) -> { bricks : List Nat, height : Nat }
getTallestBelow = \state, coords ->
    below =
        coords
        |> List.keepOks \coord -> Dict.get state.tallest coord

    height =
        below
        |> List.map .height
        |> List.max
        |> Result.withDefault 0

    bricks =
        below
        |> List.keepIf \b -> b.height == height
        |> List.map .brick

    { bricks, height }

addBrickToTower : Tower, Brick, List Nat -> (Tower, Nat)
addBrickToTower = \tower, brick, bricksBelow ->
    index = tower.bricks |> Dict.len

    bricks =
        tower.bricks
        |> Dict.insert index brick

    below =
        tower.below
        |> Dict.insert index bricksBelow

    above =
        List.walk bricksBelow tower.above \current, belowIndex ->
            Dict.update current belowIndex \entry ->
                when entry is
                    Present l -> Present (List.append l index)
                    Missing -> Present [index]

    ({ bricks, below, above }, index)
