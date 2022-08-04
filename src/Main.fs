module TestLang.Main

open TestLang.Evaluator

type Settings =
    { verbose: bool
      help: bool
      path: string option }

let defOpts =
    { help = false
      verbose = false
      path = None }

let rec parseArgs settings args =
    match args with
    | [] -> settings
    // Options
    | arg :: xs when String.length arg > 0 && arg.[0] = '-' ->
        match arg with
        | "-h"
        | "--help" -> { defOpts with help = true }
        | "-v"
        | "--verbose" -> parseArgs { settings with verbose = true } xs
        | arg ->
            failwith
                $"Got an argument I don't know what do do with: \"{arg}\" Are you sure you spelled the argument correctly? You can also run the program with -h to get a list of all available options."
    // File path
    | arg :: xs ->
        match arg.EndsWith(".tstl") with
        | true -> parseArgs { settings with path = Some arg } xs
        | false ->
            failwith
                "You specified a file but the filename doesn't end in .tstl, are you sure you provided the correct file?"

let getFileContent path =
    System.IO.File.ReadAllText(path) |> Seq.toList

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
    let settings = parseArgs defOpts (Array.toList args)

    // Show help page if requested
    if settings.help then
        printfn "TestLang - A simple language made for experimenting with interpreters"
        printfn "           and language creation."
        printfn ""
        printfn "All available options:"
        printfn "    -h, --help:     Show this help page"
        printfn "    -v, --verbose:  Show the steps along the way, useful for debugging"
        printfn "                    when something goes horribly wrong"
        exit 0

    // Get the file
    let toRun =
        match settings.path with
        | Some path -> getFileContent path
        | None -> failwith "Please specify a path for your testlang file or do -h/--help for help."

    if settings.verbose then
        printfn "Lexed:\n%A" (toRun |> Lexer.simpleLex)

    // Parse the file
    let stmts = lexParseStmt toRun

    if settings.verbose then
        printfn "AST:"
        Parser.printStatements stmts
        printfn "\nYour program:"

    // Run the file
    stmts |> (evalStmts defaultEnv) |> ignore

    0
