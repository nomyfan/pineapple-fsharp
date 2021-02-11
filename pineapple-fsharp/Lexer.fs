module Lexer

type Token =
    | EOF
    | VAR_PREFIX
    | LEFT_PAREN
    | RIGHT_PAREN
    | EQUAL
    | QUOTE
    | DUOQUOTE
    | NAME
    | PRINT
    | IGNORED

let tokenNameMap = readOnlyDict [
    Token.EOF, "EOF";
    Token.VAR_PREFIX, "$";
    Token.LEFT_PAREN, "(";
    Token.RIGHT_PAREN, ")";
    Token.EQUAL, "=";
    Token.QUOTE, "\"";
    Token.DUOQUOTE, "\"\"";
    Token.NAME, "Name";
    Token.PRINT, "print";
    Token.IGNORED, "Ignored";
]

let keywords = readOnlyDict [
    "print", Token.PRINT;
]

let regexName = System.Text.RegularExpressions.Regex "^[_\d\w]+"

type Lexer(sourceCode: string) =

    let mutable sourceCode = sourceCode
    let mutable lineNum = 1
    let mutable nextToken = ""
    let mutable nextTokenType = Token.EOF
    let mutable nextTokenLineNum = 0

    let nextSourceCodeIs (src: string) ( s: string) = src.StartsWith(s)

    member this.LineNum with get() = lineNum

    member private this.SkipSourceCode(n: int) =
        sourceCode <- sourceCode.[n..]

    member private this.IsIgnored() =

        let isNewLine (c: char) = c = '\r' || c = '\n'

        let isWhiteSpace (c: char) =
            match c with
            | '\t' | '\n' | '\v' | '\f' | '\r' | ' ' -> true
            | _ -> false

        let rec ignoreRec() =
            if sourceCode.Length = 0 then 0
            else if nextSourceCodeIs sourceCode "\r\n" || nextSourceCodeIs sourceCode "\n\r" then
                this.SkipSourceCode(2)
                lineNum <- (lineNum + 1)
                1 + ignoreRec()
            else if isNewLine(sourceCode.[0]) then
                this.SkipSourceCode(1)
                lineNum <- (lineNum + 1)
                1 + ignoreRec()
            else if isWhiteSpace(sourceCode.[0]) then
                this.SkipSourceCode(1)
                1 + ignoreRec()
            else 0
        
        ignoreRec() <> 0

    member private this.Scan(regex: System.Text.RegularExpressions.Regex) =
        let matcher = regex.Match sourceCode
        match matcher.Success with
        | true ->
            this.SkipSourceCode(matcher.Value.Length)
            matcher.Value
        | false -> raise (System.Exception("unreachable!"))

    member private this.ScanName() =
        this.Scan(regexName)

    member this.LookAhead(): Token =
        if nextTokenLineNum > 0 then nextTokenType else

        let nowLineNum = lineNum
        let (_lineNum, _tokenType, _token) = this.GetNextToken()

        lineNum <- nowLineNum
        nextTokenLineNum <- _lineNum
        nextTokenType <- _tokenType
        nextToken <- _token

        _tokenType

    member this.LookAheadAndSkip (expectedType: Token) =
        // get next token
        let nowLineNum = lineNum
        let (_lineNum, _tokenType, _token) = this.GetNextToken()

        if _tokenType = expectedType then () else
        // not is expected type, reverse cursor
        lineNum <- nowLineNum
        nextTokenLineNum <- _lineNum
        nextTokenType <- _tokenType
        nextToken <- _token

    member this.NextTokenIs (tokenType: Token) =
        let nowLineNum, nowTokenType, nowToken = this.GetNextToken()
        if tokenType = nowTokenType then (nowLineNum, nowToken)
        else raise (System.Exception(sprintf $"NextTokenIs(): syntax error near '{tokenNameMap.[nowTokenType]}', expected token: {tokenNameMap.[tokenType]} but got {tokenNameMap.[nowTokenType]}."))

    // return content before token
    member this.ScanBeforeToken (token: string) =
        let s = sourceCode.Split(token)

        if s.Length < 2 then
            raise (System.Exception("unreachable!")) 
        else
            this.SkipSourceCode(s.[0].Length)

        s.[0]

    member this.GetNextToken() =
        if nextTokenLineNum > 0 then
            let _lineNum = nextTokenLineNum
            let _tokenType = nextTokenType
            let _token = nextToken

            lineNum <- nextTokenLineNum
            nextTokenLineNum <- 0

            (_lineNum, _tokenType, _token)
        else this.MatchToken()

    member this.MatchToken() =
        let isLetter (c) = c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z'

        let matchSingleToken() =
            match sourceCode.[0] with
            | '$' ->
                this.SkipSourceCode(1)
                Some(lineNum, Token.VAR_PREFIX, tokenNameMap.[Token.VAR_PREFIX])
            | '(' ->
                this.SkipSourceCode(1)
                Some(lineNum, Token.LEFT_PAREN, tokenNameMap.[Token.LEFT_PAREN])
            | ')' ->
                this.SkipSourceCode(1)
                Some(lineNum, Token.RIGHT_PAREN, tokenNameMap.[Token.RIGHT_PAREN])
            | '=' ->
                this.SkipSourceCode(1)
                Some(lineNum, Token.EQUAL, tokenNameMap.[Token.EQUAL])
            | '"' ->
                if nextSourceCodeIs sourceCode "\"\"" then
                    this.SkipSourceCode(2)
                    Some(lineNum, Token.DUOQUOTE, tokenNameMap.[Token.DUOQUOTE])
                else
                    this.SkipSourceCode(1)
                    Some(lineNum, Token.QUOTE, tokenNameMap.[Token.QUOTE])
            | _ -> None

        let matchMultipleCharToken() =
            if sourceCode.[0] = '_' || isLetter sourceCode.[0] then
                let token = this.ScanName()
                match keywords.TryGetValue token with
                | true, tokenName -> Some(lineNum, tokenName, token)
                | false, _ -> Some(lineNum, Token.NAME, token)
            else None

        if this.IsIgnored() then
            (lineNum, Token.IGNORED, tokenNameMap.[Token.IGNORED])
        else if sourceCode.Length = 0 then
            (lineNum, Token.EOF, tokenNameMap.[Token.EOF])
        else 
            match matchSingleToken() with
            | Some(res) -> res
            | None -> 
                match matchMultipleCharToken() with
                | Some(res) -> res
                | None -> raise (System.Exception(sprintf $"MatchToken(): unexpected symbol near {sourceCode.[0].ToString()}."))