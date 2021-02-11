module Backend
open Definition
open Parser

type GlobalVaribales() =
    let variables = new System.Collections.Generic.Dictionary<string,string>()
    member this.Variables with get() = variables

let resolveAssignment(g: GlobalVaribales, assignment: Assignment)= 
    match assignment.Variable.Name with
    | "" -> Error("resolveAssignment(): variable name can NOT be empty.")
    | _ ->
        if g.Variables.ContainsKey(assignment.Variable.Name) then
            g.Variables.[assignment.Variable.Name] = assignment.String |> ignore
        else
            g.Variables.Add(assignment.Variable.Name, assignment.String)
        Ok()

let resolvePrint(g: GlobalVaribales , print: Print) =
    if print.Variable.Name = "" then Error("resolvePrint(): variable name can NOT be empty.") else

    match g.Variables.TryGetValue(print.Variable.Name) with
    | (false, _) -> Error(sprintf $"resolvePrint(): variable '${print.Variable.Name}'not found.")
    | (true, str) ->
        printfn $"{str}"
        Ok()

let resolveStatement(g: GlobalVaribales, statement: Statement) =
    match statement with
    | Statement.Assignment(assignment) -> resolveAssignment(g, assignment)
    | Statement.Print(print) -> resolvePrint(g, print)

let resolveAST(g: GlobalVaribales, ast: SourceCode) =
    match ast.Statements with
    | None -> raise(System.Exception("resolveAST(): no code to execute, please check your input."))
    | Some(stmts) ->
        let rec resolveASTRec(i: int) =
            if i >= stmts.Length then Ok() else
            match resolveStatement(g, stmts.[i]) with
            | Error(e) -> Error(e)
            | Ok() -> resolveASTRec(i + 1)

        resolveASTRec(0)

let execute(code: string) =
    match parse(code) with
    | Error(e) -> raise(System.Exception(e))
    | Ok(ast) ->
        match resolveAST(new GlobalVaribales(), ast) with
        | Error(e) -> raise(System.Exception(e))
        | Ok() -> Ok()