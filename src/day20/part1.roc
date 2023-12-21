app "advent-of-code"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.6.2/c7T4Hp8bAdWz3r9ZrhboBzibCjJag8d0IP_ljb42yVc.tar.br",
        parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.3.0/-e3ebWWmlFPfe9fYrr2z1urfslzygbtQQsl69iH1qzQ.tar.br",
    }
    imports [
        pf.Stdout,
        pf.Task.{ Task },
        parser.Core.{ const, keep, skip, map, oneOf, sepBy1, oneOrMore, chompWhile },
        parser.String.{ Utf8, parseStr, strFromUtf8, string, codeunit },
        "../../inputs/day20.txt" as inputFile : Str,
    ]
    provides [main] to pf

Parser a : Core.Parser Utf8 a

ModuleKind : [FlipFlop, Conjunction, Broadcast]

ModuleSpec : {
    name : Str,
    kind : ModuleKind,
    outputs : List Str,
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

problemParser : Parser (List ModuleSpec)
problemParser =
    oneOrMore moduleParser

moduleParser : Parser ModuleSpec
moduleParser =
    const \kind -> \name -> \outputs -> { kind, name, outputs }
    |> keep moduleKindParser
    |> keep moduleNameParser
    |> skip (string " -> ")
    |> keep (sepBy1 moduleNameParser (string ", "))
    |> skip (codeunit '\n')

moduleKindParser : Parser ModuleKind
moduleKindParser =
    oneOf [
        const FlipFlop |> skip (codeunit '%'),
        const Conjunction |> skip (codeunit '&'),
        const Broadcast,
    ]

moduleNameParser : Parser Str
moduleNameParser =
    chompWhile \ch -> ch >= 'a' && ch <= 'z'
    |> map strFromUtf8

Pulse : [Low, High]

ModuleState : [
    FlipFlop Pulse,
    Conjunction (Dict Str Pulse),
    Broadcast,
]

Module : {
    state : ModuleState,
    inputs : List Str,
    outputs : List Str,
}

FindConnectionState : {
    moduleInputs : Dict Str (List Str),
    moduleOutputs : Dict Str (List Str),
}

Signal : {
    src : Str,
    dest : Str,
    pulse : Pulse,
}

signal : Str, Str, Pulse -> Signal
signal = \src, dest, pulse ->
    { src, dest, pulse }

solveProblem : List ModuleSpec -> Nat
solveProblem = \specs ->
    mods = buildModules specs

    pulses = handleButtonPress mods 1000

    lowCount =
        pulses
        |> List.keepIf \{ pulse } -> pulse == Low
        |> List.len

    highCount = List.len pulses - lowCount

    lowCount * highCount

buildModules : List ModuleSpec -> Dict Str Module
buildModules = \specs ->
    state =
        List.walk specs { moduleInputs: Dict.empty {}, moduleOutputs: Dict.empty {} } findConnections

    specs
    |> List.map \spec -> buildModule spec state
    |> Dict.fromList

buildModule : ModuleSpec, FindConnectionState -> (Str, Module)
buildModule = \spec, { moduleInputs, moduleOutputs } ->
    inputs =
        moduleInputs
        |> Dict.get spec.name
        |> Result.withDefault []

    outputs =
        moduleOutputs
        |> Dict.get spec.name
        |> Result.withDefault []

    state =
        when spec.kind is
            FlipFlop ->
                FlipFlop Low

            Conjunction ->
                inputs
                |> List.map \input -> (input, Low)
                |> Dict.fromList
                |> Conjunction

            Broadcast ->
                Broadcast

    mod = {
        state,
        inputs,
        outputs,
    }

    (spec.name, mod)

findConnections : FindConnectionState, ModuleSpec -> FindConnectionState
findConnections = \state, spec ->
    moduleInputs =
        List.walk spec.outputs state.moduleInputs \currentInputs, output ->
            Dict.update currentInputs output \entry ->
                when entry is
                    Present list ->
                        Present (List.append list spec.name)

                    Missing ->
                        Present [spec.name]

    {
        moduleInputs,
        moduleOutputs: state.moduleOutputs |> Dict.insert spec.name spec.outputs,
    }

handleButtonPress : Dict Str Module, Nat -> List Signal
handleButtonPress = \mods, count ->
    handleButtonPressHelper mods count []

handleButtonPressHelper : Dict Str Module, Nat, List Signal -> List Signal
handleButtonPressHelper = \mods, count, seen ->
    if count == 0 then
        seen
    else
        (newMods, newPulses) = handleAllPulses mods [signal "button" "broadcaster" Low]

        handleButtonPressHelper newMods (count - 1) (List.concat seen newPulses)

handleAllPulses : Dict Str Module, List Signal -> (Dict Str Module, List Signal)
handleAllPulses = \mods, pulses ->
    handleAllPulsesHelper mods pulses pulses

handleAllPulsesHelper : Dict Str Module, List Signal, List Signal -> (Dict Str Module, List Signal)
handleAllPulsesHelper = \mods, pulses, seen ->
    if List.isEmpty pulses then
        (mods, seen)
    else
        (newMods, newPulses) =
            List.walk pulses (mods, []) \(currentMods, currentPulses), sig ->
                (nextMods, nextPulses) = handlePulse currentMods sig
                (nextMods, List.concat currentPulses nextPulses)

        handleAllPulsesHelper newMods newPulses (List.concat seen newPulses)

handlePulse : Dict Str Module, Signal -> (Dict Str Module, List Signal)
handlePulse = \mods, { src, dest, pulse } ->
    when Dict.get mods dest is
        Ok mod ->
            (state, nextPulse) = updateModuleState mod.state src pulse
            newMod = { mod & state }
            newMods = Dict.insert mods dest newMod

            pulses =
                when nextPulse is
                    NoPulse -> []
                    Pulse p ->
                        List.map newMod.outputs \output -> signal dest output p

            (newMods, pulses)

        Err _ ->
            (mods, [])

updateModuleState : ModuleState, Str, Pulse -> (ModuleState, [Pulse Pulse, NoPulse])
updateModuleState = \state, input, pulse ->
    when state is
        FlipFlop Low if pulse == Low ->
            (FlipFlop High, Pulse High)

        FlipFlop High if pulse == Low ->
            (FlipFlop Low, Pulse Low)

        FlipFlop _ ->
            (state, NoPulse)

        Conjunction mem ->
            newMem =
                Dict.insert mem input pulse

            allHigh =
                Dict.walkUntil newMem Bool.true \_, _, lastPulse ->
                    when lastPulse is
                        Low -> Break Bool.false
                        High -> Continue Bool.true

            if allHigh then
                (Conjunction newMem, Pulse Low)
            else
                (Conjunction newMem, Pulse High)

        Broadcast ->
            (state, Pulse pulse)
