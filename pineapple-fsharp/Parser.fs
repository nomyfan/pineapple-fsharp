module Parser
open Lexer
open Definition

let isSourceCodeEnd (token: Token) = token = Token.EOF

// Name ::= [_A-Za-z][_0-9A-Za-z]*
let parseName (lexer: Lexer) =
    let _, name = lexer.NextTokenIs(Token.NAME)
    
    Ok(name)

// String ::= '"' '"' Ignored | '"' StringCharacter '"' Ignored
let parseString (lexer: Lexer) =
    match lexer.LookAhead() with
    | Token.DUOQUOTE ->
        lexer.NextTokenIs(Token.DUOQUOTE) |> ignore
        lexer.LookAheadAndSkip(Token.IGNORED)
        
        Ok("")
    | Token.QUOTE ->
        lexer.NextTokenIs(Token.QUOTE) |> ignore
        let str = lexer.ScanBeforeToken(tokenNameMap.[Token.QUOTE])
        lexer.NextTokenIs(Token.QUOTE) |> ignore
        lexer.LookAheadAndSkip(Token.IGNORED)

        Ok(str)
    | _ -> Error "parseString(): not a string."

// Variable ::= "$" Name Ignored
let parseVariable (lexer: Lexer) =
    let lineNum = lexer.LineNum
    lexer.NextTokenIs(Token.VAR_PREFIX) |> ignore

    match parseName(lexer) with
    | Error(e) -> Error(e)
    | Ok(name) -> 
        lexer.LookAheadAndSkip(Token.IGNORED)
        Ok(Variable(lineNum, name))

// Assignment  ::= Variable Ignored "=" Ignored String Ignored
let parseAssignment (lexer: Lexer) =
    let lineNum = lexer.LineNum

    match parseVariable(lexer) with
    | Error(e) -> Error(e)
    | Ok(variable) ->
        lexer.LookAheadAndSkip(Token.IGNORED)
        lexer.NextTokenIs(Token.EQUAL) |> ignore
        lexer.LookAheadAndSkip(Token.IGNORED)
        match parseString(lexer) with
        | Error(e) -> Error(e)
        | Ok(str) ->
            lexer.LookAheadAndSkip(Token.IGNORED)
            Ok(new Assignment(lineNum, variable, str))

// Print ::= "print" "(" Ignored Variable Ignored ")" Ignored
let parsePrint (lexer: Lexer) =
    let lineNum = lexer.LineNum

    lexer.NextTokenIs(Token.PRINT) |> ignore
    lexer.NextTokenIs(Token.LEFT_PAREN) |> ignore
    lexer.LookAheadAndSkip(Token.IGNORED)

    match parseVariable(lexer) with
    | Error(e) -> Error(e)
    | Ok(variable) ->
        lexer.LookAheadAndSkip(Token.IGNORED)
        lexer.NextTokenIs(Token.RIGHT_PAREN) |> ignore
        lexer.LookAheadAndSkip(Token.IGNORED)
        Ok(new Print(lineNum, variable))

let parseStatement (lexer: Lexer): Result<Statement, string> =
    lexer.LookAheadAndSkip(Token.IGNORED) // skip if source code start with ignored token
    
    match lexer.LookAhead() with
    | Token.PRINT ->
        match parsePrint(lexer) with
        | Error(e) -> Error(e)
        | Ok(print) -> Ok(Print(print))
    | Token.VAR_PREFIX ->
        match parseAssignment(lexer) with
        | Error(e) -> Error(e)
        | Ok(assignment) -> Ok(Assignment(assignment))
    | _ -> Error("parseStatement(): unknown Statement.")

// Statement ::= Print | Assignment
let parseStatements (lexer: Lexer) =
    let rec parseStatementsRec(lexer: Lexer) =
        match isSourceCodeEnd(lexer.LookAhead()) with
        | true -> Ok(None)
        | false ->
            match parseStatement(lexer) with
            | Error(e) -> Error(e)
            | Ok(statement) ->
                match parseStatementsRec(lexer) with
                | Error(e) -> Error(e)
                | Ok(Some(stmts)) -> Ok(Some(Array.append [|statement|] stmts))
                | Ok(None) -> Ok(Some([|statement|]))
    
    parseStatementsRec(lexer)

// SourceCode ::= Statement+
let parseSourceCode(lexer: Lexer) =
    let lineNum = lexer.LineNum
    match parseStatements(lexer) with
    | Ok(Some(statements)) -> Ok(new SourceCode(lineNum, Some(statements)))
    | Ok(None) -> Ok(new SourceCode(lineNum, None))
    | Error(e) -> Error(e)

let parse(code: string) =
    let lexer = new Lexer(code)
    
    match parseSourceCode(lexer) with
    | Error(e) -> Error(e)
    | Ok(sourceCode) ->
        lexer.NextTokenIs(Token.EOF) |> ignore
        Ok(sourceCode)