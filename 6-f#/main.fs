open System
open System.Text.RegularExpressions

type User = { Id: int; Name: string; Age: int }


let jsonToUsers (json: string) : User list =
    let jsonObjects = json.TrimStart('[').TrimEnd(']').Split('}', StringSplitOptions.RemoveEmptyEntries)
    
    jsonObjects
    |> Array.map (fun obj ->
        let idMatch = Regex.Match(obj, @"""Id"":\s*(\d+)")
        let nameMatch = Regex.Match(obj, @"""Name"":\s*""([^""]+)""") // исправлено на правильный паттерн для строки
        let ageMatch = Regex.Match(obj, @"""Age"":\s*(\d+)")
        
        if idMatch.Success && nameMatch.Success && ageMatch.Success then
            let id = int idMatch.Groups.[1].Value
            let name = nameMatch.Groups.[1].Value
            let age = int ageMatch.Groups.[1].Value
            Some { Id = id; Name = name; Age = age }
        else
            None
    )
    |> Array.choose id 
    |> Array.toList

let usersToCsv (users: User list) : string =
    let header = "Id,Name,Age"
    let rows = 
        users 
        |> List.map (fun user -> $"{user.Id},{user.Name},{user.Age}")
        |> String.concat "\n"
    header + "\n" + rows


let json = """
[
    {"Id": 1, "Name": "Alice", "Age": 30},
    {"Id": 2, "Name": "Bob", "Age": 25},
    {"Id": 3, "Name": "Charlie", "Age": 28}
]
"""

let usersFromJson = jsonToUsers json

let csvOutput = usersToCsv usersFromJson
printfn "CSV format:\n%s" csvOutput