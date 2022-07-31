module TestLang

type Token =
    | Identifier of string
    | Int of int
    | Plus
    | Minus
    | Mul
    | Div
    | True
    | False
    | EOF

module Token =
    let requiresNextValue token =
        match token with
        | Plus -> true
        | Mul -> true
        | Minus -> true
        | Div -> true
        | _ -> false

type Expression =
    | EVar of string
    | EInt of int
    | EPlus of Expression * Expression
    | EMul of Expression * Expression
    | EDiv of Expression * Expression
    | EMinus of Expression

type Value =
    | VInt of int
    | VFloat of float

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

let rec lexIdentifier (input: char list) (pos: CodePos) (currLettersReverse: char list) =
    match input with
    | c :: xs when isLetterOrDigit c -> lexIdentifier xs (incPos 1 pos) (c :: currLettersReverse)
    | xs ->
        let str =
            List.rev currLettersReverse
            |> List.map string
            |> List.reduce (+)

        let token =
            match str with
            | "true" -> True
            | "false" -> False
            | _ -> Identifier str

        (xs, pos, token)

let rec lex (input: char list) (pos: CodePos) (tokens: FullToken list) : FullToken list =
    match input with
    | [] -> List.rev tokens
    | ' ' :: xs -> lex xs (incPos 1 pos) tokens
    | '+' :: xs -> lex xs (incPos 1 pos) (({ token = Plus; pos = pos }) :: tokens)
    | '-' :: xs ->
        // Add a plus infront of all - operators if they are between two
        // values, otherwise if there is already an operator just add the
        // subtraction.
        let shouldBeValue =
            List.tryHead tokens
            |> Option.map ((fun x -> x.token) >> Token.requiresNextValue)
            |> Option.defaultValue true

        let newTokens =
            if shouldBeValue then
                { token = Minus; pos = pos } :: tokens
            else
                { token = Minus; pos = pos }
                :: { token = Plus; pos = pos } :: tokens

        lex xs (incPos 1 pos) newTokens

    | '*' :: xs -> lex xs (incPos 1 pos) (({ token = Mul; pos = pos }) :: tokens)
    | '/' :: xs -> lex xs (incPos 1 pos) (({ token = Div; pos = pos }) :: tokens)
    | c :: xs when isDigit c ->
        let (newInput, newPos, token) = lexNumber xs (incPos 1 pos) [ c ]
        lex newInput newPos ({ token = token; pos = pos } :: tokens)
    | c :: xs when isLetter c ->
        let (newInput, newPos, token) = lexIdentifier xs (incPos 1 pos) [ c ]
        lex newInput newPos ({ token = token; pos = pos } :: tokens)
    | c :: _ -> failwith $"Unknown character {c} at position {pos}"

let simpleLex input =
    lex input { lineNo = 0; startPos = 0 } []


//////////////////
///   Parser   ///
//////////////////

let rec parseExpression (tokens: FullToken list) : Expression * FullToken list = parseSum tokens

and parseSum (tokens: FullToken list) : Expression * FullToken list =
    let expr1, tokens = parseMul None tokens

    match tokens with
    | { token = Plus } :: tokens ->
        let expr2, tokens = parseSum tokens
        EPlus(expr1, expr2), tokens
    | tokens -> expr1, tokens

and parseMul (expr1: Expression option) (tokens: FullToken list) : Expression * FullToken list =
    let expr1, tokens =
        match expr1 with
        | Some expr -> expr, tokens
        | None -> parseUnary tokens

    match tokens with
    | { token = Mul } :: xs ->
        let expr2, newTokens = parseUnary xs
        parseMul (Some(EMul(expr1, expr2))) newTokens
    | { token = Div } :: xs ->
        let expr2, newTokens = parseUnary xs
        parseMul (Some(EDiv(expr1, expr2))) newTokens
    | tokens -> expr1, tokens

and parseUnary tokens =
    match tokens with
    | { token = Minus } :: xs ->
        let expr, newTokens = parseUnary xs
        EMinus expr, newTokens
    | tokens -> parseEnd tokens

and parseEnd (tokens: FullToken list) : Expression * FullToken list =
    match tokens with
    | { token = Identifier name } :: xs -> EVar name, xs
    | { token = Int num } :: xs -> EInt num, xs
    | { token = True } :: xs -> EInt 1, xs
    | { token = False } :: xs -> EInt 0, xs
    | { token = unknown; pos = pos } :: _ ->
        failwith $"Expected a value like int, variable or float but got {unknown} at pos {pos}"
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
    | EMinus _ -> "EMinus"
    | EMul (_, _) -> "EMul"
    | EDiv (_, _) -> "EDiv"

let rec printExpression indent expr =
    let indentStr = String.replicate indent " "

    match expr with
    | EMinus expr1 ->
        printExpression (indent + 4) expr1
        printfn "%s%s" indentStr (nameOfExpression expr)
    | EPlus (expr1, expr2)
    | EDiv (expr1, expr2)
    | EMul (expr1, expr2) ->
        printExpression (indent + 4) expr1
        printfn "%s%s" indentStr (nameOfExpression expr)
        printExpression (indent + 4) expr2
    | EInt int -> printfn "%s%d" indentStr int
    | EVar var -> printfn "%s%s" indentStr var


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
        | VFloat flt1, VFloat flt2 -> VFloat(flt1 + flt2)
        | VInt int, VFloat flt
        | VFloat flt, VInt int -> VFloat(float int + flt)
    | EMinus expr1 ->
        let val1 = eval environment expr1

        match val1 with
        | VInt int1 -> VInt(-int1)
        | VFloat int1 -> VFloat(-int1)
    | EMul (expr1, expr2) ->
        let val1 = eval environment expr1
        let val2 = eval environment expr2

        match val1, val2 with
        | VInt int1, VInt int2 -> VInt(int1 * int2)
        | VFloat flt1, VFloat flt2 -> VFloat(flt1 * flt2)
        | VInt int, VFloat flt
        | VFloat flt, VInt int -> VFloat(float int * flt)
    | EDiv (expr1, expr2) ->
        let val1 = eval environment expr1
        let val2 = eval environment expr2

        match val1, val2 with
        | VInt int1, VInt int2 -> VFloat(float int1 / float int2)
        | VFloat flt1, VFloat flt2 -> VFloat(flt1 / flt2)
        | VInt int, VFloat flt -> VFloat(float int / flt)
        | VFloat flt, VInt int -> VFloat(flt / float int)
    | EVar _ -> failwith "Variables not ready"


/////////////////
///   Other   ///
/////////////////

let lexParse = Seq.toList >> simpleLex >> simpleParse
let lexParseRun = lexParse >> (eval [])

[<EntryPoint>]
let main args =
    let toRun =
        match args with
        | [||] -> failwith "Expected an argument with the code to run"
        | [| line |] -> line
        | _ -> failwith "Too many arguments. Did you forget to put double quotes: \" around the code to run?"

    printfn "Code to run: \"%s\"" toRun

    printfn "Lexed: %A" (toRun |> Seq.toList |> simpleLex)

    let expr = lexParse toRun

    printfn "AST:"
    printExpression 0 expr
    printfn ""

    lexParseRun toRun |> printfn "Result: %A"

    0
