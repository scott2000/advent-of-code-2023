app "advent-of-code"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.6.2/c7T4Hp8bAdWz3r9ZrhboBzibCjJag8d0IP_ljb42yVc.tar.br",
        parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.3.0/-e3ebWWmlFPfe9fYrr2z1urfslzygbtQQsl69iH1qzQ.tar.br",
    }
    imports [
        pf.Stdout,
        pf.Task.{ Task },
        parser.Core.{ const, keep, skip, map, between, oneOf, many, oneOrMore, chompWhile },
        parser.String.{ Utf8, parseStr, strFromUtf8, string, codeunit, digits },
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

Part : {
    x : Nat,
    m : Nat,
    a : Nat,
    s : Nat,
}

Problem : {
    rules : Dict Str Rule,
    parts : List Part,
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

problemParser : Parser Problem
problemParser =
    const \rules -> \parts -> { rules, parts }
    |> keep (oneOrMore namedRuleParser |> map Dict.fromList)
    |> skip (codeunit '\n')
    |> keep (oneOrMore partParser)

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

partParser : Parser Part
partParser =
    const \x -> \m -> \a -> \s -> { x, m, a, s }
    |> skip (string "{x=")
    |> keep digits
    |> skip (string ",m=")
    |> keep digits
    |> skip (string ",a=")
    |> keep digits
    |> skip (string ",s=")
    |> keep digits
    |> skip (string "}\n")

solveProblem : Problem -> Nat
solveProblem = \problem ->
    problem.parts
    |> List.keepIf \part -> checkPart part problem.rules == Accept
    |> List.map getRating
    |> List.sum

checkPart : Part, Dict Str Rule -> Decision
checkPart = \part, rules ->
    checkPartHelper part rules "in"

checkPartHelper : Part, Dict Str Rule, Str -> Decision
checkPartHelper = \part, rules, ruleName ->
    when Dict.get rules ruleName is
        Ok rule ->
            when evalRule rule part is
                Decision decision ->
                    decision

                Rule next ->
                    checkPartHelper part rules next

        Err _ ->
            crash "Rule not found: \(ruleName)"

evalRule : Rule, Part -> Dest
evalRule = \rule, part ->
    List.walkUntil rule.instrs rule.default \default, instr ->
        if evalPred instr.pred part then
            Break instr.dest
        else
            Continue default

evalPred : Pred, Part -> Bool
evalPred = \pred, part ->
    when pred is
        LT var val -> getVar var part < val
        GT var val -> getVar var part > val

getVar : Var, Part -> Nat
getVar = \var, part ->
    when var is
        X -> part.x
        M -> part.m
        A -> part.a
        S -> part.s

getRating : Part -> Nat
getRating = \part ->
    part.x + part.m + part.a + part.s
