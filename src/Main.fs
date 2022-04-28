open System.IO
open Lexer
open Parser
open CompilerQBE

// let openFile args =
//   if Array.isEmpty args
//   then Error "No input file specified."
//   else
//     try Ok <| File.ReadAllText args.[0]
//     with e -> Error e.Message

[<EntryPoint>]
let main argv =
    File.ReadAllText argv.[0] |> generateAST |> compile
    0
