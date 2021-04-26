// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open Domain.Person
open Domain.Project
open Domain.Testdata


let detailL =  Project.Store.storeProject
               |> getProjectDetails

printfn "%A" detailL

