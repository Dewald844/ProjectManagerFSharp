namespace Domain

open System
open Domain


type ProjectDetails = {
  Id         : int
  Name       : string
  Address    : string
  ERFNumber  : int
  ProjectFee : float
  AmountPaid : float
  DueDate    : DateTime
  Customer   : Person
  Architect  : Person
  Contractor : Person
  Finalized  : bool
}

type Project =
  | Apartment of ProjectDetails
  | Store     of ProjectDetails
  | Building  of ProjectDetails

module Project =

  let createProject (projtype : string)(id : int) (name : string)(address : string) (erf: int)(fee: float) (paid: float)(date: DateTime) (cust: Domain.Person)(arch: Domain.Person)(cont : Domain.Person) : Project =
    match projtype with
      | "Apartment" ->
          Project.Apartment {
            ProjectDetails.Id = id
            ProjectDetails.Name = name
            ProjectDetails.Address = address
            ProjectDetails.ERFNumber = erf
            ProjectDetails.ProjectFee = fee
            ProjectDetails.AmountPaid = paid
            ProjectDetails.DueDate = date
            ProjectDetails.Architect = arch
            ProjectDetails.Customer = cust
            ProjectDetails.Contractor = cont
            ProjectDetails.Finalized = false
          }
      | "Store" ->
          Project.Store {
            ProjectDetails.Id = id
            ProjectDetails.Name = name
            ProjectDetails.Address = address
            ProjectDetails.ERFNumber = erf
            ProjectDetails.ProjectFee = fee
            ProjectDetails.AmountPaid = paid
            ProjectDetails.DueDate = date
            ProjectDetails.Architect = arch
            ProjectDetails.Customer = cust
            ProjectDetails.Contractor = cont
            ProjectDetails.Finalized = false
          }
      | "Building" ->
          Project.Building {
            ProjectDetails.Id = id
            ProjectDetails.Name = name
            ProjectDetails.Address = address
            ProjectDetails.ERFNumber = erf
            ProjectDetails.ProjectFee = fee
            ProjectDetails.AmountPaid = paid
            ProjectDetails.DueDate = date
            ProjectDetails.Architect = arch
            ProjectDetails.Customer = cust
            ProjectDetails.Contractor = cont
            ProjectDetails.Finalized = false
          }
