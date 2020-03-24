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

let plusP = pcharV '+' Plus
let minusP = pcharV '-' Minus

let addP = pcharV '+' Add
let subP = pcharV '-' Sub
let mulP = pcharV '*' Mul
let divP = pcharV '/' Div
let powP = pcharV '^' Pow
let modP = pcharV '%' Mod

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

let expression, expressionRef = createParserForwardedToRef<Expression, UserState>()

let numberP =
  floatP
  |>> Expression.fromFloat

let constantP =
  piP <|> eP
  |>> Expression.fromConstant
  <?> "constant"

let parenthesesP =
  between
    (leftParenP .>> spaces)
    (rightParenP .>> spaces)
    (expression .>> spaces)

let unaryP =
  plusP <|> minusP
  .>>. expression
  .>> spaces
  |>> Unary
  <?> "unary operator"

let binaryP =
  expression
  .>> spaces
  .>>. choice [ addP; subP; mulP; divP; powP; modP ]
  .>> spaces
  .>>. expression
  .>> spaces
  |>> (fun ((e1, b), e2) -> Binary (b, e1, e2))
  <?> "binary expression"

let functionP =
  choice [ sinP; cosP; tanP; log10P; logP; absP; signP; sqrtP; roundP ]
  .>>. parenthesesP
  .>> spaces
  |>> Function
  <?> "function"

expressionRef :=
  spaces
  >>. choice [ numberP; constantP; parenthesesP; unaryP; binaryP; functionP ]

let parse = run expression
