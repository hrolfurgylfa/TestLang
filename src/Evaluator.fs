module TestLang.Evaluator

open TestLang.Parser

type Value =
    | VInt of int
    | VFloat of float

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
    | EVar name -> getEnvVar name environment
