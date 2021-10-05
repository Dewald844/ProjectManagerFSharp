namespace Domain

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