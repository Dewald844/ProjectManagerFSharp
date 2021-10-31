namespace Domain

open System
open Domain
open Types.Helpers

type ProjectBuildingType =
  | House
  | ApartmentState
  | Store
  | Warehouse

type ProjectReferences = {
  Number   : ProjectId
  Address  : PhysicalAddress
  Customer : Person.State
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
type ProjectTeam = Person.State * Person.State

type ProjectData = {
  References : ProjectReferences * ProjectTeam
  Details    : ProjectDetails    * ProjectFinance
}
type State =
  | NonExisting
  | Existing  of ProjectData
  | Finalized of ProjectData

type Project = {ProjectState : State}

type UpdateMethod =
  | ChangeDeadline of DateTime
  | PaymentMade    of CashAmount
  | Finalize

type Command =
  | Create of ProjectReferences * ProjectDetails * ProjectFinance * ProjectTeam
  | Update of UpdateMethod

type SuccessfulEvent =
  | Created          of ProjectData
  | DeadlineUpdated  of Project * SystemDate
  | PaymentUpdated   of Project * CashAmount
  | ProjectFinalized of Project

type FailedEvent =
  | CreateFailed           of Project * Exception
  | DeadlineUpdateFailed   of Project * Exception
  | PaymentUpdateFailed    of Project * Exception
  | ProjectFinalizedFailed of Project * Exception

type Event = Result<SuccessfulEvent,FailedEvent>

module Command =

  let interpretCommand (command : Command ) (project : Project) : Event =

    match command with
    | Create (projectReferences, details, finance, team) ->
      match project.ProjectState with
      | NonExisting ->
        let newProject =
          { References = (projectReferences, team)
            Details    = (details, finance) }
        Ok (SuccessfulEvent.Created <| newProject)
      | _ ->
        Error (FailedEvent.CreateFailed <| (project,
                ($"Can not create project with any state other than Non-Existing REF : {project}" |> Exception)))
    | Command.Update method ->
      match method with
      | ChangeDeadline newDate ->
        match project.ProjectState with
        | Existing _projectData ->
           let newSystemDate = newDate |> SystemDate.System
           Ok (SuccessfulEvent.DeadlineUpdated <| (project, newSystemDate))
        | NonExisting      ->
          Error
            (FailedEvent.DeadlineUpdateFailed <| (project,
              ($"Can not update non existing project REF : {project}" |> Exception)))
        | Finalized _project ->
          Error
            (FailedEvent.DeadlineUpdateFailed <| (project,
              ($"No need to change an finalized project's deadline REF : {project}" |> Exception)))
      | PaymentMade paidAmount ->
        match project.ProjectState with
        | Existing projectData ->
          let _,finance =
            projectData.Details
          if finance.IsPayedInFull then
            Error
              (FailedEvent.PaymentUpdateFailed <| (project,
                ($"Cannot receive payment of project that is payed in full REF {project}" |> Exception)))
          else
            Ok (SuccessfulEvent.PaymentUpdated <| (project, paidAmount))
        | Finalized projectData ->
          let _,finance =
            projectData.Details
          if finance.IsPayedInFull then
            Error
              (FailedEvent.PaymentUpdateFailed <| (project,
                ($"Cannot receive payment of project that is payed in full REF {project}" |> Exception)))
          else
            Ok (SuccessfulEvent.PaymentUpdated <| (project, paidAmount))
        | NonExisting ->
          Error
            (FailedEvent.PaymentUpdateFailed <| (project,
              ($"Cannot update payment of non existing project REF : {project}" |> Exception)))
      | Finalize  ->
        match project.ProjectState with
        | Existing _projectData ->
          Ok (SuccessfulEvent.ProjectFinalized <| project)
        | _ ->
          Error
            (FailedEvent.ProjectFinalizedFailed <| (project,
              ($"Can only finalize existing project REF {project}" |> Exception)))


module Event =

  let evolveEvent (event: Event) : Project =
    match event with
    | Ok successFullEvent ->
      match successFullEvent with
      | Created newProject -> {ProjectState = Existing <| newProject}
      | DeadlineUpdated (project, newDate) ->
        match project.ProjectState with
        | Existing currentData ->
          let currentDetails, currentFinance = currentData.Details
          let newDetails = ({currentDetails with Deadline = newDate}, currentFinance)
          let evolvedData = {currentData with Details = newDetails}
          {project with ProjectState = (Existing <| evolvedData)}
        | _ -> failwith $"Should be unreachable REF : {event}"
      | PaymentUpdated (project, payment) ->
        match project.ProjectState with
        | Existing currentData ->
          let currentDetails,currentFinance = currentData.Details
          let newDetails =
               (currentDetails,
                 {currentFinance with PaidAmount = (currentFinance.PaidAmount
                                                      |> CashAmount.addCashAmount payment )})
          let evolvedData = { currentData with Details = newDetails }
          { project with ProjectState = (Existing <| evolvedData) }
        | Finalized currentData ->
          let currentDetails,currentFinance = currentData.Details
          let newDetails =
               (currentDetails,
                 {currentFinance with PaidAmount = (currentFinance.PaidAmount
                                                      |> CashAmount.addCashAmount payment )})
          let evolvedData = { currentData with Details = newDetails }
          { project with ProjectState = (Finalized <| evolvedData) }
        | _ -> failwith $"Should be unreachable REF : {event}"
      | ProjectFinalized project ->
        match project.ProjectState with
        | Existing currentData ->
          let currentDetails, currentFinance = currentData.Details
          let newDetails = ({currentDetails with IsFinalised = true}, currentFinance)
          let evolvedData = {currentData with Details = newDetails}
          { project with ProjectState = (Finalized <| evolvedData) }
        | _ -> failwith $"Should be unreachable REF : {event}"
    | Error failedEvent ->
      match failedEvent with
      | CreateFailed (_ , message) ->
        failwith $"Failed to create project REF : {message}"
      | DeadlineUpdateFailed (_ , message) ->
        failwith $"Failed to update deadline REF : {message}"
      | PaymentUpdateFailed (_ , message) ->
        failwith $"Failed to update payment REF : {message}"
      | ProjectFinalizedFailed (_ , message) ->
        failwith $"Failed to finalize project REF : {message}"