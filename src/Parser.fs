module TestLang.Parser

open TestLang.Lexer

type Expression =
    | EVar of string
    | EInt of int
    | EPlus of Expression * Expression
    | EMul of Expression * Expression
    | EDiv of Expression * Expression
    | EMinus of Expression

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
    | { token = LBracket } :: xs ->
        let expr, newTokens = parseSum xs

        match newTokens with
        | { token = RBracket } :: xs2 -> expr, xs2
        | { token = unknown; pos = pos } :: _ ->
            failwith $"Expected a closing bracket but instead found {unknown} at pos {pos}"
        | [] -> failwith $"Expected a closing bracket but instead the file ended."
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
