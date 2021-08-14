namespace Domain

open System
open Domain

type ProjectNumber = Number of int
type ProjectName = Name of string
type ProjectAddress = Address of string
type ProjectErfNumber = ErfNumber of int
type ProjectTotalCost = Amount of float
type ProjectCostPaid = Amount of float
type ProjectDeadline = Date of DateTime

type ProjectBuildingType =
  | House
  | ApartmentState
  | Store
  | Warehouse

type Project = {
  Number       : ProjectNumber
  Name         : ProjectName
  BuildingType : ProjectBuildingType
  Address      : ProjectAddress
  ErfNumber    : ProjectErfNumber
  Deadline     : ProjectDeadline
  TotalCost    : ProjectTotalCost
  PaidAmount   : ProjectCostPaid
  Architect    : Person
  Contractor   : Person
  Customer     : Person
  IsFinalised  : bool
  IsPayedInFull : bool
}

module Project =

  let createProject
    (number : ProjectNumber) (name : ProjectName)
    (building : ProjectBuildingType) (address : ProjectAddress)
    (erf : ProjectErfNumber) (deadline : ProjectDeadline)
    (cost : ProjectTotalCost) (architect : Person)
    (contractor : Person) (customer : Person) =
    {
      Number = number
      Name = name
      BuildingType = building
      Address = address
      ErfNumber = erf
      Deadline = deadline
      TotalCost = cost
      PaidAmount = float 0  |> ProjectCostPaid.Amount
      Architect =
        if architect |> Person.isArchitect
        then architect
        else failwith $"Project Architect cannot be {architect |> Person.getRole |> Person.roleToString}"
      Contractor =
        if contractor |> Person.isContractor
        then contractor
        else failwith $"Project Architect cannot be {contractor |> Person.getRole |> Person.roleToString}"
      Customer =
        if customer |> Person.isCustomer
        then customer
        else failwith $"Project Architect cannot be {customer |> Person.getRole |> Person.roleToString}"
      IsFinalised = false
      IsPayedInFull = false
    }

  let payedAmountToFloat (paidAmount : ProjectCostPaid) =
    match paidAmount with
    | Amount amount -> amount

  let totalCostToFloat (totalCost : ProjectTotalCost) =
    match totalCost with
    | ProjectTotalCost.Amount cost -> cost

  let updateDeadline (project : Project) (newDeadline : DateTime) =
    {project with Deadline = newDeadline |> ProjectDeadline.Date }

  let payedInFullCheck (project : Project) =
    if (project.TotalCost |> totalCostToFloat) = (project.PaidAmount |> payedAmountToFloat)
    then
      {project with IsPayedInFull = true}
    else
      {project with IsPayedInFull = false}

  let updatePayment (project : Project) (amountPayed : float) =
    {project with PaidAmount = ((project.PaidAmount |> payedAmountToFloat) + amountPayed) |> ProjectCostPaid.Amount }
    |> payedInFullCheck