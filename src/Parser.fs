module TestLang.Parser

open TestLang.Lexer

type Expression =
    | EVar of string
    | EInt of int
    | EPlus of Expression * Expression
    | EMul of Expression * Expression
    | EDiv of Expression * Expression
    | EMinus of Expression

type Statement =
    | SPrint of Expression
    | SExpr of Expression
    | SLet of string * Expression

let rec parseStatement (tokens: FullToken list) : Statement * FullToken list =
    match tokens with
    | { token = Print } :: xs ->
        let expr, xs2 = parseExpression xs
        SPrint expr, xs2
    | { token = Let; pos = letPos } :: xs ->
        // Get the variable name
        let iden, xs2 =
            match xs with
            | { token = Identifier name } :: xs2 -> name, xs2
            | { token = unknown } :: _ ->
                failwith $"Expecting a variable name after let here {letPos} but instead found {unknown}"
            | [] -> failwith $"Expecting a variable name after let here {letPos} but instead the program ended"

        // Get the variable value and return the statement
        match xs2 with
        | { token = Assign } :: xs3 ->
            let fromExpr, xs4 = parseExpression xs3
            SLet(iden, fromExpr), xs4
        | { pos = pos } :: _ ->
            failwith
                $"Expected a value to be assigned to the let biding here {pos} but I didn't find any equals sign, did you forget to assign a value to this variable?"
        | [] ->
            failwith
                $"Expected to find an equals sign with the let binding at {letPos} but instead the program ended, did you forget to assign a value to your variable?"
    | xs ->
        let expr, xs2 = parseExpression xs
        SExpr expr, xs2

and parseExpression (tokens: FullToken list) : Expression * FullToken list = parseSum tokens

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

let simpleParseStmts tokens =
    let rec parseStmts stmts tokens =
        let stmt, tokens = parseStatement tokens

        match tokens with
        | [] -> List.rev (stmt :: stmts)
        | xs -> parseStmts (stmt :: stmts) xs

    parseStmts [] tokens

let nameOfExpression expr =
    match expr with
    | EVar _ -> "EVar"
    | EInt _ -> "EInt"
    | EPlus (_, _) -> "EPlus"
    | EMinus _ -> "EMinus"
    | EMul (_, _) -> "EMul"
    | EDiv (_, _) -> "EDiv"

let rec expressionToStr expr =
    match expr with
    | EMinus expr1 ->
        let expr1Str = expressionToStr expr1
        sprintf "(%s %s)" (nameOfExpression expr) expr1Str
    | EPlus (expr1, expr2)
    | EDiv (expr1, expr2)
    | EMul (expr1, expr2) ->
        let expr1Str = expressionToStr expr1
        let expr2Str = expressionToStr expr2
        sprintf "(%s %s %s)" (nameOfExpression expr) expr1Str expr2Str
    | EInt int -> sprintf "(int %d)" int
    | EVar var -> sprintf "(getVar %s)" var

let rec printExpression expr = printfn "%s" (expressionToStr expr)

let statementToStr stmt =
    match stmt with
    | SPrint expr -> sprintf "Print: %s" (expressionToStr expr)
    | SExpr expr -> sprintf "Expression: %s" (expressionToStr expr)
    | SLet (iden, fromExpr) -> sprintf "Assigning %s to the variable \"%s\"" (expressionToStr fromExpr) iden

let printStatements stmts =
    stmts
    |> List.map statementToStr
    |> String.concat "\n"
    |> printfn "%s"
