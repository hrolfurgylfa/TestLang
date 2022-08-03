module TestLang.Lexer

type Token =
    | Identifier of string
    | Int of int
    | Plus
    | Minus
    | Mul
    | Div
    | True
    | False
    | LBracket
    | RBracket
    | Print
    | Let
    | Assign

module Token =
    let requiresNextValue token =
        match token with
        | Plus -> true
        | Mul -> true
        | Minus -> true
        | Div -> true
        | _ -> false

type CodePos = { lineNo: int; startPos: int }

type FullToken = { token: Token; pos: CodePos }

let incPos amount pos =
    { pos with startPos = pos.startPos + amount }

open System

let isDigit char = Char.IsDigit(char)

let isLetter char = Char.IsLetter(char)

let isLetterOrDigit char = Char.IsLetterOrDigit(char)

let rec lexNumber (input: char list) (pos: CodePos) (currDigitsReverse: char list) =
    match input with
    | c :: xs when isDigit c -> lexNumber xs (incPos 1 pos) (c :: currDigitsReverse)
    | xs ->
        let num =
            List.rev currDigitsReverse
            |> List.map string
            |> List.reduce (+)
            |> int

        (xs, pos, Int num)

let rec lexIdentifier (input: char list) (pos: CodePos) (currLettersReverse: char list) =
    match input with
    | c :: xs when isLetterOrDigit c -> lexIdentifier xs (incPos 1 pos) (c :: currLettersReverse)
    | xs ->
        let str =
            List.rev currLettersReverse
            |> List.map string
            |> List.reduce (+)

        let token =
            match str with
            | "true" -> True
            | "false" -> False
            | "print" -> Print
            | "let" -> Let
            | _ -> Identifier str

        (xs, pos, token)

let rec lex (input: char list) (pos: CodePos) (tokens: FullToken list) : FullToken list =
    match input with
    | [] -> List.rev tokens
    | ' ' :: xs
    | '\t' :: xs -> lex xs (incPos 1 pos) tokens
    | '\r' :: xs -> lex xs pos tokens
    | '\n' :: xs ->
        let newPos =
            { startPos = 1
              lineNo = pos.lineNo + 1 }

        lex xs newPos tokens
    | '+' :: xs -> lex xs (incPos 1 pos) (({ token = Plus; pos = pos }) :: tokens)
    | '-' :: xs ->
        // Add a plus infront of all - operators if they are between two
        // values, otherwise if there is already an operator just add the
        // subtraction.
        let shouldBeValue =
            List.tryHead tokens
            |> Option.map ((fun x -> x.token) >> Token.requiresNextValue)
            |> Option.defaultValue true

        let newTokens =
            if shouldBeValue then
                { token = Minus; pos = pos } :: tokens
            else
                { token = Minus; pos = pos }
                :: { token = Plus; pos = pos } :: tokens

        lex xs (incPos 1 pos) newTokens

    | '*' :: xs -> lex xs (incPos 1 pos) (({ token = Mul; pos = pos }) :: tokens)
    | '/' :: xs -> lex xs (incPos 1 pos) (({ token = Div; pos = pos }) :: tokens)
    | '(' :: xs -> lex xs (incPos 1 pos) (({ token = LBracket; pos = pos }) :: tokens)
    | ')' :: xs -> lex xs (incPos 1 pos) (({ token = RBracket; pos = pos }) :: tokens)
    | '=' :: xs -> lex xs (incPos 1 pos) (({ token = Assign; pos = pos }) :: tokens)
    | c :: xs when isDigit c ->
        let (newInput, newPos, token) = lexNumber xs (incPos 1 pos) [ c ]
        lex newInput newPos ({ token = token; pos = pos } :: tokens)
    | c :: xs when isLetter c ->
        let (newInput, newPos, token) = lexIdentifier xs (incPos 1 pos) [ c ]
        lex newInput newPos ({ token = token; pos = pos } :: tokens)
    | c :: _ -> failwith $"Unknown character {c} at position {pos}"

let simpleLex input =
    lex input { lineNo = 1; startPos = 1 } []
