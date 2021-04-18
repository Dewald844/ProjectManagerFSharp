// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open Domain.Person
open Domain.Project

// Define a function to construct a message to print
let name = "Dewald Haasbroek"
let email = "dewald844@gmail.com"
let address = "P33 Voelvlei str"
let phone = 0814136792
let personType = "Customer"

let cust1 = 
    createPerson personType name email address phone

printfn "First Customer %s" (cust1.ToString())