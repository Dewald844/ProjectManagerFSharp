namespace Domain

open Types
open Types.Helpers

type ProjectBuildingType =
  | House
  | ApartmentState
  | Store
  | Warehouse

type ProjectReferences = {
  Number           : ProjectNumber
  Address          : PhysicalAddress
  Customer         : Person.Person
}
type ProjectFinance = {
  TotalCost     : CashAmount
  PaidAmount    : CashAmount
  IsPayedInFull : bool
}
type ProjectDetails = {
  BuildingType     : ProjectBuildingType
  ProjectErfNumber : ERFNumber
  Deadline         : SystemDate
  IsFinalised      : bool
}
type ProjectTeam = Person.Person * Person.Person

type Project = {
  References : ProjectReferences * ProjectTeam
  Details    : ProjectDetails    * ProjectFinance
}
type State =
  | NonExisting
  | Existing  of Project
  | Finalized of Project