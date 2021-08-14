namespace Domain

type PersonName = Name of string
type TelephoneNumber = Number of int
type EmailAddress = Email of string
type PhysicalAddress = Address of string

type PersonRole =
  | Architect
  | Contractor
  | Customer

type Person ={
  Name    : PersonName
  Number  : TelephoneNumber
  Email   : EmailAddress
  Address : PhysicalAddress
  Role    : PersonRole
}

module Person =

  let createPerson
      (name : string)
      (number : int)
      (email : string)
      (address : string)
      (role : PersonRole) =
    {
      Name    = name    |> PersonName.Name
      Number  = number  |> TelephoneNumber.Number
      Email   = email   |> EmailAddress.Email
      Address = address |> PhysicalAddress.Address
      Role    = role
    }

  let getName (person : Person) =
    person.Name

  let getNumber (person : Person) =
    person.Number

  let getEmail (person : Person) =
    person.Email

  let getAddress (person : Person) =
    person.Address

  let getRole (person : Person) =
     let role = person.Role
     match role with
     | Architect -> PersonRole.Architect
     | Contractor -> PersonRole.Contractor
     | Customer -> PersonRole.Customer