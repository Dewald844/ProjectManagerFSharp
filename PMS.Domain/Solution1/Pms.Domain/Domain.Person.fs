namespace Domain

open System.Net.NetworkInformation
open Types
open Types.Helpers

[<RequireQualifiedAccess>]

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
  type Person =
   | Architect of PersonDetails
   | Contractor of PersonDetails
   | Customer  of PersonDetails


  let createPerson
    (personType : string) (newId : int)
    (firstname : string) ( lastName : string)
    (phoneNumber : int) (email : string)
    (houseNumber : int) (streetName : string)
    (cityName : string) : Person =

    let address =
      ((houseNumber |> HouseNumber.createAddressNumber)
       ,(streetName |> StreetName.createStreetName)
       ,(cityName   |> City.createCity))

    let contactDetails =
      {
        TelephoneNumber = phoneNumber |> TelephoneNumber.createTelephoneNumber
        EmailAddress    = email |> EmailAddress.Email
        PhysicalAddress = address
      }

    let details =
      {
        PersonId = newId |> Int99.createInt99
        Name =
          ((firstname |> FirstName.createFirstName),
           (lastName |> LastName.createLastName))
        ContactDetails = contactDetails
      }

    match personType with
    | "Architect" -> Architect details
    | "Contractor" -> Contractor details
    | "Customer"   -> Customer details
    | _ -> failwith $"Invalid person type input REF {personType}"