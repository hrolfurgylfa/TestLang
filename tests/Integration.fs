module Integration

open Xunit

open TestLang

let assertParse result code = Assert.Equal(result, lexParseRun code)

[<Fact>]
let simpleMath () =
    assertParse (VInt 4) "1 + 3"
    assertParse (VInt 16) "1 + 3 * 5"
    assertParse (VInt 17) "5 * 3 + 2"
    assertParse (VInt 11) "1 + 5 * 3- 3-1  *2"
