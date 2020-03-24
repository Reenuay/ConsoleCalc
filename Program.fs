module ConsoleCalc.Progam

open System
open FParsec

[<EntryPoint>]
let main _ =
  printfn "Welcome to console calc!"

  let rec iter () =
    printfn "Please enter a new expression:"

    let input = Console.ReadLine()
    match Parser.parse input with
    | Success (e, _, _) -> Expression.solve e |> printfn "Result: %f"
    | Failure (e, _, _) -> printfn "Error: %s" e

    iter ()

  iter ()
  
  0 // return an integer exit code
