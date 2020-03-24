module ConsoleCalc.Parser

open System
open FParsec

// --- Utilities ---

type UserState = unit
type Parser<'a> = Parser<'a, UserState>

let mayThrow (p: Parser<'a>) : Parser<'a> =
  fun stream ->
    let state = stream.State
    try 
      p stream
    with e -> // catching all exceptions is somewhat dangerous
      stream.BacktrackTo(state)
      Reply(FatalError, messageError e.Message)

let pcharV char value : Parser<_>  = pchar char >>% value

let pstringV str value : Parser<_>  = pstring str >>% value

// --- Primitive parsers ---

let numberFormat =
  NumberLiteralOptions.AllowFraction
  ||| NumberLiteralOptions.AllowExponent

let floatP : Parser<_> =
  numberLiteral numberFormat "number"
  |>> (fun n -> float n.String)
  |> mayThrow

let leftParenP : Parser<_> = pchar '('

let rightParenP : Parser<_> = pchar ')'

let piP = pstringV "pi" PI
let eP = pcharV 'e' E

let sinP   = pstringV "sin" Sin
let cosP   = pstringV "cos" Cos
let tanP   = pstringV "tan" Tan
let log10P = pstringV "log10" Log10
let logP   = pstringV "log" Log
let absP   = pstringV "abs" Abs
let signP  = pstringV "sign" Sign
let sqrtP  = pstringV "sqrt" Sqrt
let roundP = pstringV "round" Round

// --- Expression parsers ---

let opp = new OperatorPrecedenceParser<Expression,unit,unit>()
let expression = opp.ExpressionParser

let numberP =
  floatP
  .>> spaces
  |>> Expression.fromFloat

let constantP =
  piP <|> eP
  .>> spaces
  |>> Expression.fromConstant
  <?> "constant"

let parenthesesP =
  between
    (leftParenP .>> spaces)
    (rightParenP .>> spaces)
    expression
  .>> spaces

let functionP =
  choice [ sinP; cosP; tanP; log10P; logP; absP; signP; sqrtP; roundP ]
  .>>. parenthesesP
  |>> Function
  <?> "function"

opp.TermParser <- choice [ numberP; constantP; functionP; parenthesesP ]

opp.AddOperator(InfixOperator("+", spaces, 1, Associativity.Left, Expression.fromBinary Add))
opp.AddOperator(InfixOperator("-", spaces, 1, Associativity.Left, Expression.fromBinary Sub))
opp.AddOperator(InfixOperator("*", spaces, 2, Associativity.Left, Expression.fromBinary Mul))
opp.AddOperator(InfixOperator("/", spaces, 2, Associativity.Left, Expression.fromBinary Div))
opp.AddOperator(InfixOperator("^", spaces, 3, Associativity.Right, Expression.fromBinary Pow))
opp.AddOperator(InfixOperator("%", spaces, 2, Associativity.Left, Expression.fromBinary Mod))
opp.AddOperator(PrefixOperator("+", spaces, 4, true, Expression.fromUnary Plus))
opp.AddOperator(PrefixOperator("-", spaces, 4, true, Expression.fromUnary Minus))

let parser = spaces >>. expression .>> eof

let parse = run parser
