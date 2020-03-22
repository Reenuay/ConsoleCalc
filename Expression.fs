module ConsoleCalc

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
