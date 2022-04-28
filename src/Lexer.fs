module Lexer

type Token =
    | INCREMENT
    | DECREMENT
    | SHIFT_LEFT
    | SHIFT_RIGHT
    | OUTPUT
    | INPUT
    | OPEN_LOOP
    | CLOSE_LOOP

let lexer (input: string) =
    let tok (chs: char) =
        match chs with
        | '+' -> Some INCREMENT
        | '-' -> Some DECREMENT
        | '<' -> Some SHIFT_LEFT
        | '>' -> Some SHIFT_RIGHT
        | '.' -> Some OUTPUT
        | ',' -> Some INPUT
        | '[' -> Some OPEN_LOOP
        | ']' -> Some CLOSE_LOOP
        | _ -> None

    let rec tokenizer (chars: char list) =
        match chars with
        | x :: xs ->
            match tok x with
            | Some t -> t :: tokenizer xs
            | None -> tokenizer xs
        | _ -> []

    input |> Seq.toList |> tokenizer
