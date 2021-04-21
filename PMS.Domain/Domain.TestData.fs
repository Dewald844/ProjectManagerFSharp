namespace Domain

open Domain.Person
open Domain.Project
open System

module Testdata =
// Define a function to construct a message to print
  module Person =
    module Customer =
      let name = "Dewald Haasbroek"
      let email = "dewald844@gmail.com"
      let address = "P33 Voelvlei str"
      let phone = 0814136792
      let personType = "Customer"
      let cust1 = createPerson personType name email address phone

    module Architect =

      let name = "Lenard Haasbroek"
      let email = "lenard@gmail.com"
      let address = "21 veldevie Paarl"
      let phone = 0744172222
      let personType = "Architect"
      let arch1 = createPerson personType name email address phone

    module Contractor =

      let name = "Johan slabbert"
      let email = "JSlabbert@gmail.com"
      let address = "41 Turnberry lodge"
      let phone = 0846579823
      let personType = "Contractor"
      let cont1 = createPerson personType name email address phone


  module Project =
    module Store =
      let projectType = "Store"
      let id = 1
      let name = "Fairbridge"
      let address = "Old Paarl Road"
      let erfNum = 5568477
      let fee = 85000.00
      let amountpaid = 0.0
      let duedate = DateTimeOffset.Now
      let cust = Person.Customer.cust1
      let arch = Person.Architect.arch1
      let cont = Person.Contractor.cont1

      let storeProject = createProject projectType id name address erfNum fee amountpaid duedate cust arch cont
