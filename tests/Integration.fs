module Integration

open Xunit

open TestLang.Main
open TestLang.Lexer
open TestLang.Evaluator

let assertParse result code =
    Assert.Equal(result, code |> Seq.toList |> lexParseRun)

let assertParseStmt result code =
    Assert.Equal(result, code |> Seq.toList |> lexParseRunStmt)

[<Fact>]
let addAndMul () =
    assertParse (VInt 4) "1 + 3"
    assertParse (VInt 16) "1 + 3 * 5"
    assertParse (VInt 17) "5 * 3 + 2"

[<Fact>]
let subtract () =
    assertParse (VInt 11) "1 + 5 * 3- 3-1  *2"
    assertParse (VInt 3) "--3"
    assertParse (VInt -3) "---3"
    assertParse (VInt -3) "---3"
    assertParse (VInt -9) "3 * -3"
    assertParse (VInt -8) "1 + -3 * 3"
    assertParse (VInt -8) "1 - 3 * 3"

[<Fact>]
let divide () =
    assertParse (VFloat 3.0) "6 / 2"
    assertParse (VFloat 3.5) "7 / 2"
    assertParse (VFloat 4.0) "8 / 2"
    assertParse (VFloat 17.5) "15 / 6 * 7"
    assertParse (VFloat 113.75) "15 / 6 * 7 / 3 / 4 * 78"

[<Fact>]
let brackets () =
    assertParse (VInt 102) "51 + 3 * 17"
    assertParse (VInt 918) "(51 + 3) * 17"
    assertParse (VFloat 93.5) "51 / 2 + 4 * 17"
    assertParse (VFloat 144.5) "51 / (2 + 4) * 17"

[<Fact>]
let booleans () =
    assertParse (VInt 2) "true + true + false"
    assertParse (VInt 1) "true + false - true + true"

[<Fact>]
let variables () =
    assertParse (VFloat 6.28) "pi * 2"
    assertParse (VInt 61025937) "veryRandomConstant"
    assertParse (VInt 122051880) "((veryRandomConstant) + 3) * 2"

let getFstTokenPos str =
    str
    |> Seq.toList
    |> simpleLex
    |> (fun xs -> xs.[0].pos)

[<Fact>]
let lexPosInfo () =
    Assert.Equal({ lineNo = 1; startPos = 1 }, getFstTokenPos "true")
    Assert.Equal({ lineNo = 4; startPos = 7 }, getFstTokenPos "   \n  \n \n      true  ")

[<Fact>]
let stmtReturn () =
    assertParseStmt VVoid "print 3"
    assertParseStmt VVoid "let a = 3"
    assertParseStmt (VInt 3) "3"

[<Fact>]
let createVariables () =
    assertParseStmt (VInt 3) "let a = 1\nlet b = 2\na + b"
    assertParseStmt (VFloat 80.5) "let a = 5 * (32 - 1) / 2 + 1\nlet b = 2\na + b"
