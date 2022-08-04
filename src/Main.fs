module TestLang.Main

open TestLang.Evaluator

let defaultEnv =
    Map.empty
    |> Map.add "pi" (VFloat 3.14)
    |> Map.add "version" (VInt 0)
    |> Map.add "veryRandomConstant" (VInt 61025937)

let lexStr = Seq.toList >> Lexer.simpleLex
let lexParse = lexStr >> Parser.simpleParse
let lexParseStmt = lexStr >> Parser.simpleParseStmts
let lexParseRunStmt = lexParseStmt >> evalStmts defaultEnv
let lexParseRun = lexParse >> eval defaultEnv

[<EntryPoint>]
let main args =
    let toRun =
        match args with
        | [||] -> failwith "Expected an argument with the code to run"
        | [| line |] -> line
        | _ -> failwith "Too many arguments. Did you forget to put double quotes: \" around the code to run?"

    printfn "Code to run: \"%s\"" toRun
    printfn ""

    printfn "Lexed:\n%A" (toRun |> Seq.toList |> Lexer.simpleLex)

    let stmts = lexParseStmt toRun

    printfn "AST:"
    Parser.printStatements stmts
    printfn ""

    printfn "Your program:"
    stmts |> (evalStmts defaultEnv) |> ignore

    0
