// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open Domain.Person
open Domain.Project
open Domain.Testdata

printfn "First %s" (Person.Customer.cust1.ToString())
printfn "First %s" (Project.Store.storeProject.ToString())