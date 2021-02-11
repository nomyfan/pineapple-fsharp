module Definition

type Variable (lineNum: int, name: string) =
    member this.LineNum = lineNum
    member this.Name = name

type Assignment (lineNum: int, variable: Variable, str: string) =
    member this.LineNum = lineNum
    member this.Variable = variable
    member this.String = str

type Print (lineNum: int, variable: Variable) =
    member this.LineNum = lineNum
    member this.Variable = variable

type Statement =
    | Print of Print
    | Assignment of Assignment

type SourceCode(lineNum: int, statements: Statement array option) =
    member this.LineNum = lineNum
    member this.Statements = statements