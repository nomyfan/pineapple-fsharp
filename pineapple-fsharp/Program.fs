// Learn more about F# at http://fsharp.org

open Backend

[<EntryPoint>]
let main argv =
    if argv.Length < 1 then
        printfn "Usage: dotnet run filename"
        1
    else
        let filename = argv.[0]
        let code =
            try 
                System.IO.File.ReadAllText(filename) |> Some
            with
            | _ ->
                printfn "Error reading file: %s" filename
                None
        
        match code with
        | None -> 1
        | Some(code) ->
            match execute(code) with
            | Error(e) -> printfn e
            | Ok() -> 0
