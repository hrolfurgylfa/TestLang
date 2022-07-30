module TestLang

type Token =
    | Identifier of string
    | Int of int
    | Plus
    | Minus
    | Mul
    | True
    | False
    | EOF

type Expression =
    | EVar of string
    | EInt of int
    | EPlus of Expression * Expression
    | EMul of Expression * Expression
    | EMinus of Expression * Expression

type Value = VInt of int

type CodePos = { lineNo: int; startPos: int }

type FullToken = { token: Token; pos: CodePos }

let incPos amount pos =
    { pos with startPos = pos.startPos + amount }

open System

let isDigit char = Char.IsDigit(char)

let isLetter char = Char.IsLetter(char)

let isLetterOrDigit char = Char.IsLetterOrDigit(char)


/////////////////
///   Lexer   ///
/////////////////

let rec lexNumber (input: char list) (pos: CodePos) (currDigitsReverse: char list) =
    match input with
    | c :: xs when isDigit c -> lexNumber xs (incPos 1 pos) (c :: currDigitsReverse)
    | xs ->
        let num =
            List.rev currDigitsReverse
            |> List.map string
            |> List.reduce (+)
            |> int

        (xs, pos, Int num)

let rec lexDigitOrLetter (input: char list) (pos: CodePos) (currLettersReverse: char list) =
    match input with
    | c :: xs when isLetterOrDigit c -> lexDigitOrLetter xs (incPos 1 pos) (c :: currLettersReverse)
    | xs ->
        let str =
            List.rev currLettersReverse
            |> List.map string
            |> List.reduce (+)

        (xs, pos, Identifier str)

let rec lex (input: char list) (pos: CodePos) (tokens: FullToken list) : FullToken list =
    match input with
    | [] -> List.rev tokens
    | ' ' :: xs -> lex xs (incPos 1 pos) tokens
    | '+' :: xs -> lex xs (incPos 1 pos) (({ token = Plus; pos = pos }) :: tokens)
    | '-' :: xs -> lex xs (incPos 1 pos) (({ token = Minus; pos = pos }) :: tokens)
    | '*' :: xs -> lex xs (incPos 1 pos) (({ token = Mul; pos = pos }) :: tokens)
    | 't' :: 'r' :: 'u' :: 'e' :: xs -> lex xs (incPos 4 pos) (({ token = True; pos = pos }) :: tokens)
    | 'f' :: 'a' :: 'l' :: 's' :: 'e' :: xs -> lex xs (incPos 5 pos) (({ token = False; pos = pos }) :: tokens)
    | c :: xs when isDigit c ->
        let (newInput, newPos, token) = lexNumber xs (incPos 1 pos) [ c ]
        lex newInput newPos ({ token = token; pos = pos } :: tokens)
    | c :: xs when isLetter c ->
        let (newInput, newPos, token) = lexDigitOrLetter xs (incPos 1 pos) [ c ]
        lex newInput newPos ({ token = token; pos = pos } :: tokens)
    | c :: _ -> failwith $"Unknown character {c} at position {pos}"

let simpleLex input =
    lex input { lineNo = 0; startPos = 0 } []


//////////////////
///   Parser   ///
//////////////////

let rec parseExpression (tokens: FullToken list) : Expression * FullToken list = parseSum tokens

and parseSum (tokens: FullToken list) : Expression * FullToken list =
    let expr1, tokens = parseMul tokens

    match tokens with
    | { token = Plus } :: tokens ->
        let expr2, tokens = parseSum tokens
        EPlus(expr1, expr2), tokens
    | { token = Minus } :: tokens ->
        let expr2, tokens = parseSum tokens
        EMinus(expr1, expr2), tokens
    | tokens -> expr1, tokens

and parseMul (tokens: FullToken list) : Expression * FullToken list =
    let expr1, tokens = parseEnd tokens

    match tokens with
    | { token = Mul } :: tokens ->
        let expr2, tokens = parseMul tokens
        EMul(expr1, expr2), tokens
    | tokens -> expr1, tokens

and parseEnd (tokens: FullToken list) : Expression * FullToken list =
    match tokens with
    | { token = Identifier name } :: xs -> EVar name, xs
    | { token = Int num } :: xs -> EInt num, xs
    | { token = True } :: xs -> EInt 1, xs
    | { token = False } :: xs -> EInt 0, xs
    | { token = unknown } :: _ -> failwith $"Expected a value like int, variable or float but got {unknown}"
    | [] ->
        failwith
            "Tokens list was empty when trying to find a value. This is an error in the internals of the interpreter."

let simpleParse tokens =
    let expr, tokens = parseExpression tokens

    match tokens with
    | [] -> expr
    | _ -> failwith $"Still have tokens after expression, these should have all been used: {tokens}"

let nameOfExpression expr =
    match expr with
    | EVar _ -> "EVar"
    | EInt _ -> "EInt"
    | EPlus (_, _) -> "EPlus"
    | EMinus (_, _) -> "EMinus"
    | EMul (_, _) -> "EMul"

let rec printExpression indent expr =
    let indentStr = String.replicate indent " "

    match expr with
    | EPlus (expr1, expr2)
    | EMinus (expr1, expr2)
    | EMul (expr1, expr2) ->
        printExpression (indent + 4) expr1
        printf "%s%A\n" indentStr (nameOfExpression expr)
        printExpression (indent + 4) expr2
    | EInt int -> printf "%s%d\n" indentStr int
    | EVar var -> printf "%s%s\n" indentStr var


/////////////////////
///   Evaluator   ///
/////////////////////

let rec eval environment expr =
    match expr with
    | EInt int -> VInt int
    | EPlus (expr1, expr2) ->
        let val1 = eval environment expr1
        let val2 = eval environment expr2

        match val1, val2 with
        | VInt int1, VInt int2 -> VInt(int1 + int2)
    | EMinus (expr1, expr2) ->
        let val1 = eval environment expr1
        let val2 = eval environment expr2

        match val1, val2 with
        | VInt int1, VInt int2 -> VInt(int1 - int2)
    | EMul (expr1, expr2) ->
        let val1 = eval environment expr1
        let val2 = eval environment expr2

        match val1, val2 with
        | VInt int1, VInt int2 -> VInt(int1 * int2)
    | EVar _ -> failwith "Variables not ready"

[<EntryPoint>]
let main args =
    let toRun =
        match args with
        | [||] -> failwith "Expected an argument with the code to run"
        | [| line |] -> line
        | _ -> failwith "Too many arguments. Did you forget to put double quotes: \" around the code to run?"

    printfn "%A" toRun

    let expr = toRun |> Seq.toList |> simpleLex |> simpleParse

    // printExpression 0 expr

    expr |> eval [] |> printfn "%A"

    0
