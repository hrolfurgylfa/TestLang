module TestLang.Main

open TestLang.Evaluator

let defaultEnv =
    Map.empty
    |> Map.add "pi" (VFloat 3.14)
    |> Map.add "version" (VInt 0)
    |> Map.add "veryRandomConstant" (VInt 61025937)

let lexParse =
    Seq.toList
    >> Lexer.simpleLex
    >> Parser.simpleParse

let lexParseRun = lexParse >> (eval defaultEnv)

[<EntryPoint>]
let main args =
    let toRun =
        match args with
        | [||] -> failwith "Expected an argument with the code to run"
        | [| line |] -> line
        | _ -> failwith "Too many arguments. Did you forget to put double quotes: \" around the code to run?"

    printfn "Code to run: \"%s\"" toRun

    printfn "Lexed: %A" (toRun |> Seq.toList |> Lexer.simpleLex)

    let expr = lexParse toRun

    printfn "AST:"
    Parser.printExpression 0 expr
    printfn ""

    expr |> (eval defaultEnv) |> printfn "Result: %A"

    0
