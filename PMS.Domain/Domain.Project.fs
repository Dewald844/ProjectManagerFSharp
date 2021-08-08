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
  DueDate    : DateTimeOffset
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

  let createProject (projtype : string)(id : int) (name : string)(address : string) (erf: int)(fee: float) (paid: float)(date: DateTimeOffset) (cust: Domain.Person)(arch: Domain.Person)(cont : Domain.Person) : Project =
    match projtype with
      | "Apartment" ->
          Apartment {
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
          Store {
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
          Building {
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

  let getProjectDetails (project : Project) = 
    match project with 
    | Apartment d -> 
      [d.Id.ToString()
       d.Name.ToString()
       d.Address.ToString() 
       d.ERFNumber.ToString()
       d.ProjectFee.ToString() 
       d.AmountPaid.ToString()
       d.DueDate.ToString()
       d.Finalized.ToString()]
    | Building  d -> 
      [d.Id.ToString() 
       d.Name.ToString()
       d.Address.ToString() 
       d.ERFNumber.ToString()
       d.ProjectFee.ToString() 
       d.AmountPaid.ToString()
       d.DueDate.ToString()
       d.Finalized.ToString()] 
    | Store     d -> 
      [d.Id.ToString() 
       d.Name.ToString()
       d.Address.ToString() 
       d.ERFNumber.ToString()
       d.ProjectFee.ToString() 
       d.AmountPaid.ToString()
       d.DueDate.ToString()
       d.Finalized.ToString()]