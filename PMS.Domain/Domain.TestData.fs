namespace Domain

open Domain.Person
open Domain.Project
module Testdata =
// Define a function to construct a message to print
  module Person =
    let name = "Dewald Haasbroek"
    let email = "dewald844@gmail.com"
    let address = "P33 Voelvlei str"
    let phone = 0814136792
    let personType = "Customer"

    let cust1 = createPerson personType name email address phone