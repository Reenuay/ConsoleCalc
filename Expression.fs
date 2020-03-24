namespace ConsoleCalc

open System

type Constant = PI | E

type Unary = Plus | Minus

type Binary =
  | Add | Sub
  | Mul | Div
  | Mod | Pow

type Function =
  | Sin   | Cos  | Tan
  | Log10 | Log  | Abs
  | Sign  | Sqrt | Round

type Expression =
  | Number of float
  | Constant of Constant
  | Unary of Unary * Expression
  | Binary of Binary * Expression * Expression
  | Function of Function * Expression

[<RequireQualifiedAccess>]
module Constant =
  let toFloat = function
  | PI -> Math.PI
  | E  -> Math.E

[<RequireQualifiedAccess>]
module Unary =
  let toFunc : Unary -> float -> float = function
  | Plus  -> fun i -> i
  | Minus -> fun i -> -i

[<RequireQualifiedAccess>]
module Binary =
  let toFunc = function
  | Add -> (+)
  | Sub -> (-)
  | Mul -> (*)
  | Div -> (/)
  | Pow -> ( ** )
  | Mod -> (%)

[<RequireQualifiedAccess>]
module Function =
  let toFunc = function
  | Sin   -> Math.Sin
  | Cos   -> Math.Cos
  | Tan   -> Math.Tan
  | Log10 -> Math.Log10
  | Log   -> Math.Log
  | Abs   -> Math.Abs
  | Sign  -> Math.Sign >> float
  | Sqrt  -> Math.Sqrt
  | Round -> Math.Round

[<RequireQualifiedAccess>]
module Expression =
  let fromFloat = Number

  let fromConstant = Constant

  let rec solve = function
  | Number f           -> f
  | Constant c         -> Constant.toFloat c
  | Unary (u, e)       -> Unary.toFunc u (solve e)
  | Binary (b, e1, e2) -> Binary.toFunc b (solve e1) (solve e2)
  | Function (f, e)    -> Function.toFunc f (solve e)
