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
        Customer {
          Details.Name = name
          Details.Email = email
          Details.Address = address
          Details.PhoneNumber = phoneNum
        }
    | "Architect" ->
        Architect {
          Details.Name = name
          Details.Email = email
          Details.Address = address
          Details.PhoneNumber = phoneNum
        }
    | "Contractor" ->
        Contractor {
          Details.Name = name
          Details.Email = email
          Details.Address = address
          Details.PhoneNumber = phoneNum
        }
