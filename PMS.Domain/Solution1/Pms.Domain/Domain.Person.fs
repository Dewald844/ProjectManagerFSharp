namespace Domain

open System
open Types
open Types.Helpers

module Person =

  type EmailAddress    = Email of string

  type ContactDetails = {
    TelephoneNumber : TelephoneNumber
    EmailAddress    : EmailAddress
    PhysicalAddress : PhysicalAddress
  }

  type PersonDetails ={
    PersonId       : Int99
    Name           : FirstName * LastName
    ContactDetails : ContactDetails
  }

  type PersonType =
    | Architect
    | Contractor
    | Customer

  type PersonData =
    | Architect  of PersonDetails
    | Contractor of PersonDetails
    | Customer   of PersonDetails

  type State =
    | NonExisting
    | Existing of PersonData

  type UpdateMethod =
    | UpdateTelephoneNumber of TelephoneNumber
    | UpdateEmail           of EmailAddress
    | UpdatePhysicalAddress of PhysicalAddress

  type Command =
    | Create of PersonType * PersonDetails
    | Update of UpdateMethod

  type SuccessFullEvent =
    | Created                of State * PersonData
    | TelephoneNumberUpdated of State * PersonData
    | EmailUpdated           of State * PersonData
    | AddressUpdated         of State * PersonData

  type FailedEvent =
    | CreateFailed                of State * Exception
    | TelephoneNumberUpdateFailed of State * Exception
    | EmailUpdateFailed           of State * Exception
    | AddressUpdateFailed         of State * Exception

  type Event = Result<SuccessFullEvent,FailedEvent>

  module Command =

    let interpretCommand (personState : State) (command : Command) : Event =
      match command with
      | Create (personType, personDetails) ->
        match personState with
        | NonExisting ->
          let newPersonData =
            match personType with
            | PersonType.Architect  -> Architect <| personDetails
            | PersonType.Contractor -> Contractor <| personDetails
            | PersonType.Customer   -> Customer <| personDetails
          Ok (SuccessFullEvent.Created <| (personState, newPersonData))
        | Existing _ ->
          Error (FailedEvent.CreateFailed <| (personState,($"Can not create person with an existing state REF {personState}" |> Exception)))
      | Update method ->
        match method with
        | UpdateTelephoneNumber newNumber ->
          match personState with
          | Existing personData ->
            match personData with
            | Architect currentData ->
              let newContactDetails = {currentData.ContactDetails with TelephoneNumber = newNumber }
              let newData = {currentData with ContactDetails = newContactDetails}
              let newPersonData = Architect <| newData
              Ok (SuccessFullEvent.TelephoneNumberUpdated <| (personState, newPersonData))
            | Contractor currentData ->
              let newContactDetails = {currentData.ContactDetails with TelephoneNumber = newNumber }
              let newData = {currentData with ContactDetails = newContactDetails}
              let newPersonData = Contractor <| newData
              Ok (SuccessFullEvent.TelephoneNumberUpdated <| (personState, newPersonData))
            | Customer currentData ->
              let newContactDetails = {currentData.ContactDetails with TelephoneNumber = newNumber }
              let newData = {currentData with ContactDetails = newContactDetails}
              let newPersonData = Customer <| newData
              Ok (SuccessFullEvent.TelephoneNumberUpdated <| (personState, newPersonData))
          | NonExisting ->
            Error (FailedEvent.TelephoneNumberUpdateFailed <| (personState, ($"Can not update an non existing person's telephone number REF {personState}" |> Exception)))
        | UpdateEmail newEmail ->
          match personState with
          | Existing personData ->
            match personData with
            | Architect currentData ->
              let newContactDetails = {currentData.ContactDetails with EmailAddress = newEmail }
              let newData = {currentData with ContactDetails = newContactDetails}
              let newPersonData = Architect <| newData
              Ok (SuccessFullEvent.EmailUpdated <| (personState, newPersonData))
            | Contractor currentData ->
              let newContactDetails = {currentData.ContactDetails with EmailAddress = newEmail }
              let newData = {currentData with ContactDetails = newContactDetails}
              let newPersonData = Contractor <| newData
              Ok (SuccessFullEvent.EmailUpdated <| (personState, newPersonData))
            | Customer currentData ->
              let newContactDetails = {currentData.ContactDetails with EmailAddress = newEmail }
              let newData = {currentData with ContactDetails = newContactDetails}
              let newPersonData = Architect <| newData
              Ok (SuccessFullEvent.EmailUpdated <| (personState, newPersonData))
          | NonExisting ->
            Error (FailedEvent.EmailUpdateFailed <| (personState, ($"Can not update an non existing person's Email REF {personState}" |> Exception)))