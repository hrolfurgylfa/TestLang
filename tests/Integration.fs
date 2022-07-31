module Integration

open Xunit

open TestLang

let assertParse result code = Assert.Equal(result, lexParseRun code)

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
