module Parser

open Lexer

type Expr =
    | INC
    | DEC
    | SR
    | SL
    | OUT
    | INP
    | LOOP of list<Expr>

type AST = list<Expr> // type aliasing

type State = STATE of (List<AST> * List<Token>)

let translateToken (tk: Token) =
    match tk with
    | INCREMENT -> INC
    | DECREMENT -> DEC
    | SHIFT_RIGHT -> SR
    | SHIFT_LEFT -> SL
    | OUTPUT -> OUT
    | INPUT -> INP

let updateStack (e: Expr) (s: State) =
    match s with
    | STATE (x :: xs, tokens) -> STATE((x @ [ e ]) :: xs, tokens)

let rec parser (s: State) =
    match s with
    | STATE (stack, x :: xs) ->
        match x with
        | OPEN_LOOP -> parser (STATE([] :: stack, xs))
        | CLOSE_LOOP ->
            match stack with
            | y :: ys -> parser (updateStack (LOOP y) (STATE(ys, xs)))
            | _ -> None
        | _ -> parser (updateStack (translateToken x) (STATE(stack, xs)))
    | STATE (_, _) -> Some s


let generateAST (str: string) =
    let tokens = lexer str
    let stateParser = parser (STATE([ [] ], tokens))

    match stateParser with
    | Some (STATE (AST, _)) ->
        if List.length AST > 1 then
            failwithf "Malformed Expression"
        else
            List.head AST
    | _ -> failwithf "Error while parsing expressions"
