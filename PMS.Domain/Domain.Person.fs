namespace Domain

type Details = {
  Name        : string
  Email       : string
  Address     : string
  PhoneNumber : int
}

type Person =
  | Customer   of Details
  | Architect  of Details
  | Contractor of Details

module Person =

  let createPerson (persontype: string)(name : string) (email : string)(address : string) (phoneNum : int) : Person =
    match persontype with
    | "Customer" ->
        Person.Customer {
          Details.Name = name
          Details.Email = email
          Details.Address = address
          Details.PhoneNumber = phoneNum
        }
    | "Architect" ->
        Person.Architect {
          Details.Name = name
          Details.Email = email
          Details.Address = address
          Details.PhoneNumber = phoneNum
        }
    | "Contractor" ->
        Person.Contractor {
          Details.Name = name
          Details.Email = email
          Details.Address = address
          Details.PhoneNumber = phoneNum
        }
