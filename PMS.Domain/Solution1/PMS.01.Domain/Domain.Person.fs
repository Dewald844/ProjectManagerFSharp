namespace Domain

open System
open Types.Helpers

module Person =

  type EmailAddress = Email of string

  type PersonType =
    | Architect
    | Contractor
    | Customer

  type ContactDetails = {
    TelephoneNumber : TelephoneNumber
    EmailAddress    : EmailAddress
    PhysicalAddress : PhysicalAddress
  }

  type PersonDetails ={
    PersonId       : Int99
    Name           : FirstName * LastName
    ContactDetails : ContactDetails
    PersonType     : PersonType
  }

  type State =
    | NonExisting
    | Existing of PersonDetails

  type Person = { PersonState : State }

  type UpdateMethod =
    | UpdateTelephoneNumber of TelephoneNumber
    | UpdateEmail           of EmailAddress
    | UpdatePhysicalAddress of PhysicalAddress

  type Command =
    | Create of PersonDetails
    | Update of UpdateMethod

  type SuccessFullEvent =
    | Created                of State * PersonDetails
    | TelephoneNumberUpdated of State * PersonDetails
    | EmailUpdated           of State * PersonDetails
    | AddressUpdated         of State * PersonDetails

  type FailedEvent =
    | CreateFailed                of State * Exception
    | TelephoneNumberUpdateFailed of State * Exception
    | EmailUpdateFailed           of State * Exception
    | AddressUpdateFailed         of State * Exception

  type Event = Result<SuccessFullEvent,FailedEvent>


  module Command =

    let interpretCommand (state : State) (command :  Command) : Event =
      match command with
      | Create person ->
        match state with
        | NonExisting ->
          Ok (SuccessFullEvent.Created (state , person))
        | Existing _ ->
          Error (FailedEvent.CreateFailed <|( state, ("Cannot create and person with existing state" |> Exception)))
      | Update method ->
        match method with
        | UpdateTelephoneNumber newNumber ->
          match state with
          | Existing person ->
            let newContactDetails = { person.ContactDetails with TelephoneNumber = newNumber}
            let newPersonDetails  = { person with ContactDetails = newContactDetails }
            Ok (SuccessFullEvent.TelephoneNumberUpdated <| (state, newPersonDetails))
          | NonExisting ->
            Error (TelephoneNumberUpdateFailed <| (state, Exception $"Cannot update non existing person telephone number REF : {state}"))
        | UpdateEmail newEmail ->
          match state with
          | Existing person ->
            let newContactDetails = { person.ContactDetails with EmailAddress = newEmail }
            let newPersonDetails  = { person with ContactDetails = newContactDetails }
            Ok (SuccessFullEvent.EmailUpdated <| (state, newPersonDetails))
          | NonExisting ->
            Error (EmailUpdateFailed <| (state, Exception $"Cannot update non existing person email address REF : {state}"))
        | UpdatePhysicalAddress newAddress ->
          match state with
          | Existing person ->
            let newContactDetails = { person.ContactDetails with PhysicalAddress = newAddress }
            let newPersonDetails  = { person with ContactDetails = newContactDetails }
            Ok ( SuccessFullEvent.AddressUpdated <| (state , newPersonDetails))
          | NonExisting ->
            Error (AddressUpdateFailed <| (state, Exception $"Cannot update non existing person address REF : {state}"))


  module Event =

    let evolveEvent (event : Event) : Person =
      match event with
      | Ok se ->
        match se with
        | SuccessFullEvent.Created (state , person) ->
          match state with
          | NonExisting -> { PersonState = Existing <| person }
          | Existing _ -> unreachable()
        | SuccessFullEvent.TelephoneNumberUpdated (state, person) ->
          match state with
          | Existing _person -> { PersonState = Existing <| person }
          | NonExisting -> unreachable()
        | SuccessFullEvent.EmailUpdated (state, person) ->
          match state with
          | Existing _ -> {PersonState = Existing <| person}
          | NonExisting -> unreachable()
        | SuccessFullEvent.AddressUpdated (state, person) ->
          match state with
          | Existing _ -> { PersonState = Existing <| person }
          | NonExisting -> unreachable()
      | Error  fe ->
        match fe with
        | FailedEvent.CreateFailed (state, reason) -> failwithf $"{reason} REF : {state}"
        | FailedEvent.TelephoneNumberUpdateFailed( state, reason) -> failwithf $"{reason} REF : {state}"
        | FailedEvent.EmailUpdateFailed (state , reason) -> failwithf $"{reason} REF : {state}"
        | FailedEvent.AddressUpdateFailed (state , reason) -> failwithf $"{reason} REF : {state}"