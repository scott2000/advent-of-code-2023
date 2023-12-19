app "advent-of-code"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.6.2/c7T4Hp8bAdWz3r9ZrhboBzibCjJag8d0IP_ljb42yVc.tar.br",
        parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.3.0/-e3ebWWmlFPfe9fYrr2z1urfslzygbtQQsl69iH1qzQ.tar.br",
    }
    imports [
        pf.Stdout,
        pf.Task.{ Task },
        parser.Core.{ const, keep, skip, map, between, oneOf, many, oneOrMore, chompWhile },
        parser.String.{ Utf8, parseStr, strFromUtf8, anyString, codeunit, digits },
        "../../inputs/day19.txt" as inputFile : Str,
    ]
    provides [main] to pf

Parser a : Core.Parser Utf8 a

Var : [X, M, A, S]

Decision : [
    Accept,
    Reject,
]

Dest : [
    Decision Decision,
    Rule Str,
]

Pred : [
    LT Var Nat,
    GT Var Nat,
]

Instr : {
    pred : Pred,
    dest : Dest,
}

Rule : {
    instrs : List Instr,
    default : Dest,
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

problemParser : Parser (Dict Str Rule)
problemParser =
    const Dict.fromList
    |> keep (oneOrMore namedRuleParser)
    |> skip (codeunit '\n')
    |> skip anyString

namedRuleParser : Parser (Str, Rule)
namedRuleParser =
    const \name -> \rule -> (name, rule)
    |> keep ruleNameParser
    |> keep (between ruleParser (codeunit '{') (codeunit '}'))
    |> skip (codeunit '\n')

ruleParser : Parser Rule
ruleParser =
    const \instrs -> \default -> { instrs, default }
    |> keep (many instrParser)
    |> keep destParser

instrParser : Parser Instr
instrParser =
    const \pred -> \dest -> { pred, dest }
    |> keep predParser
    |> skip (codeunit ':')
    |> keep destParser
    |> skip (codeunit ',')

predParser : Parser Pred
predParser =
    const \var -> \op -> \dest -> op var dest
    |> keep varParser
    |> keep opParser
    |> keep digits

opParser : Parser (Var, Nat -> Pred)
opParser =
    oneOf [
        const (\v, d -> LT v d) |> skip (codeunit '<'),
        const (\v, d -> GT v d) |> skip (codeunit '>'),
    ]

varParser : Parser Var
varParser =
    oneOf [
        const X |> skip (codeunit 'x'),
        const M |> skip (codeunit 'm'),
        const A |> skip (codeunit 'a'),
        const S |> skip (codeunit 's'),
    ]

destParser : Parser Dest
destParser =
    oneOf [
        map decisionParser Decision,
        map ruleNameParser Rule,
    ]

decisionParser : Parser Decision
decisionParser =
    oneOf [
        const Accept |> skip (codeunit 'A'),
        const Reject |> skip (codeunit 'R'),
    ]

ruleNameParser : Parser Str
ruleNameParser =
    chompWhile \ch -> ch >= 'a' && ch <= 'z'
    |> map strFromUtf8

Part : {
    x : List Nat,
    m : List Nat,
    a : List Nat,
    s : List Nat,
}

solveProblem : Dict Str Rule -> Nat
solveProblem = \rules ->
    validRange = List.range { start: At 1, end: Length 4000 }

    initialPart = {
        x: validRange,
        m: validRange,
        a: validRange,
        s: validRange,
    }

    countPossibilities initialPart rules "in"

countPossibilities : Part, Dict Str Rule, Str -> Nat
countPossibilities = \part, rules, ruleName ->
    when Dict.get rules ruleName is
        Ok rule ->
            (defaultPart, count) =
                List.walk rule.instrs (part, 0) \(currentPart, currentCount), instr ->
                    result =
                        filterPartByPred currentPart instr.pred

                    (result.drop, currentCount + countPossibilitiesForDest result.keep rules instr.dest)

            count + countPossibilitiesForDest defaultPart rules rule.default

        Err _ ->
            crash "Rule not found: \(ruleName)"

countPossibilitiesForDest : Part, Dict Str Rule, Dest -> Nat
countPossibilitiesForDest = \part, rules, dest ->
    when dest is
        Rule ruleName ->
            countPossibilities part rules ruleName

        Decision Accept ->
            countPartPossibilities part

        Decision Reject ->
            0

countPartPossibilities : Part -> Nat
countPartPossibilities = \part ->
    [part.x, part.m, part.a, part.s]
    |> List.map List.len
    |> List.product

filterPartByPred : Part, Pred -> { keep : Part, drop : Part }
filterPartByPred = \part, pred ->
    when pred is
        LT var b -> filterPart part var \a -> a < b
        GT var b -> filterPart part var \a -> a > b

filterPart : Part, Var, (Nat -> Bool) -> { keep : Part, drop : Part }
filterPart = \part, var, filter ->
    update =
        when var is
            X -> \f -> { part & x: f part.x }
            M -> \f -> { part & m: f part.m }
            A -> \f -> { part & a: f part.a }
            S -> \f -> { part & s: f part.s }

    {
        keep: update \list -> List.keepIf list filter,
        drop: update \list -> List.dropIf list filter,
    }
