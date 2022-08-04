module TestLang.Evaluator

open TestLang.Parser

type Value =
    | VInt of int
    | VFloat of float
    | VVoid

let toStr value =
    match value with
    | VInt int -> int.ToString()
    | VFloat flt -> flt.ToString()
    | VVoid -> "void"

let getEnvVar (var: string) env : Value =
    let res = Map.tryFind var env

    match res with
    | Some x -> x
    | None -> failwith $"Variable \"{var}\" could not be found, are you sure it was spelled correctly?"

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
        | val1, val2 -> failwith $"Can't add {val1} and {val2}"
    | EMinus expr1 ->
        let val1 = eval environment expr1

        match val1 with
        | VInt int1 -> VInt(-int1)
        | VFloat int1 -> VFloat(-int1)
        | VVoid -> failwith $"Can't invert a Void value"
    | EMul (expr1, expr2) ->
        let val1 = eval environment expr1
        let val2 = eval environment expr2

        match val1, val2 with
        | VInt int1, VInt int2 -> VInt(int1 * int2)
        | VFloat flt1, VFloat flt2 -> VFloat(flt1 * flt2)
        | VInt int, VFloat flt
        | VFloat flt, VInt int -> VFloat(float int * flt)
        | val1, val2 -> failwith $"Can't multiply {val1} and {val2}"
    | EDiv (expr1, expr2) ->
        let val1 = eval environment expr1
        let val2 = eval environment expr2

        match val1, val2 with
        | VInt int1, VInt int2 -> VFloat(float int1 / float int2)
        | VFloat flt1, VFloat flt2 -> VFloat(flt1 / flt2)
        | VInt int, VFloat flt -> VFloat(float int / flt)
        | VFloat flt, VInt int -> VFloat(flt / float int)
        | val1, val2 -> failwith $"Can't devide {val1} with {val2}"
    | EVar name -> getEnvVar name environment

let rec evalStmts env stmts =
    match stmts with
    | [] -> VVoid
    | SPrint expr :: xs ->
        let result = eval env expr
        printfn "%s" (toStr result)
        evalStmts env xs
    | SExpr expr :: xs ->
        let res = eval env expr

        match xs with
        | [] -> res
        | _ -> evalStmts env xs
    | SLet (iden, expr) :: xs ->
        let newEnv = Map.add iden (eval env expr) env
        evalStmts newEnv xs
