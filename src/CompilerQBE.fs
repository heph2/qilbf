module CompilerQBE

open Lexer
open Parser

let prologue () =
  "export\n" +
  "function w $main() {\n" +
  "@start\n" +
  "   %ptr =l alloc16 4096\n" +
  "   %r =l call $memset(l %ptr, l 0, l 4096)\n"
  
let epilogue () =
  "@end\n" +
  "   ret 0\n}\n"

let rec toQBE (ast: AST) (h: int) =
  match ast with
    | x::xs ->
      match x with
        | INC ->
          let str, h' = (toQBE xs h)
          ("   %v =l loadl %ptr\n" +
           "   %v =l add %v, 1\n" +
           "   storel %v, %ptr\n" +
           str, h')
          
        | DEC ->
          let str, h' = (toQBE xs h)
          ("   %v =l loadl %ptr\n" +
           "   %v =l add %v, -1\n" + 
           "   storel %v, %ptr\n" +
           str, h')

        | SR ->
          let str, h' = (toQBE xs h)
          (" %ptr =l add %ptr, 8\n" +
           str, h')

        | SL ->
          let str, h' = (toQBE xs h)
          (" %ptr =l add %ptr, -8\n" +
           str, h')

        | LOOP (x) ->
          let body, h' = (toQBE x (h + 3))
          let str, h'' = (toQBE xs h')
          ("@loop" + string h + "\n" +
           "  %v =l loadl %ptr\n" +
           "  jnz %v, @loop" + string (h + 1) + ", @loop" + string (h + 2) + "\n" +
           "@loop" + string (h + 1) + "\n" +
           body +
           "jmp @loop" + string (h) + "\n" +
           "@loop" + string (h + 2) + "\n" +
           str, h'')
    
        | INP ->
          let str, h' = (toQBE xs h)
          ("   %v =l loadl %ptr\n" +
           "   %r =w call $getchar()\n" +
           str, h')

        | OUT ->
          let str, h' = (toQBE xs h)
          ("   %v =l loadl %ptr\n" + 
           "   %r =l call $putchar(l %v)\n" +
           str, h')
          
        | _ ->
          let str, h' = (toQBE xs h)
          (" /* nothing */\n " +          
           str, h')
          
    | _ -> ("",h)

let compile (ast: AST) =
  let str, _ = toQBE (ast) (1)
  let qbeSourceCode = prologue() + str + epilogue()
  printfn "%s" qbeSourceCode
